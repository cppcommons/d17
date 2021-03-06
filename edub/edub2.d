// https://code.dlang.org/package-format?lang=json
module main;
import std.algorithm : canFind, startsWith, endsWith;
import std.array : empty, join, split, replace;
import std.file : chdir, copy, dirEntries, exists, getcwd, mkdirRecurse, read,
	rename, remove, rmdirRecurse, setTimes, write, FileException,
	PreserveAttributes, SpanMode;
import std.format : format;
import std.json;
import std.path : absolutePath, baseName, dirName, extension, isAbsolute,
	relativePath;
import std.process : execute, executeShell;
import std.regex : regex, matchAll, matchFirst, replaceAll;
import std.stdio : writefln, writeln, stdout, File;
import std.typecons : Yes, No;
import std.datetime.systime : Clock;
import std.process : pipeProcess, spawnProcess, wait, Redirect;
import std.string : strip;
import std.uni : toLower;
import std.uuid : sha1UUID, UUID;

private void exit(int code)
{
	import std.c.stdlib;

	std.c.stdlib.exit(code);
}

class EDubContext
{
	string fullPath;
	string dirName;
	string fileName;
	string extension;
	string baseName;
	string basePath;
	string[] path_keyword_list = [
		"targetPath", "sourceFiles", "sourcePaths", "excludedSourceFiles",
		"mainSourceFile", "copyFiles", "importPaths", "stringImportPaths"
	];
	bool opt_cleanup;
	this()
	{
	}
}

private __gshared EDubContext g_context = new EDubContext();

private string make_abs_path(string path)
{
	string[] split_donwload_path(string arg)
	{
		int prefix_len(string arg)
		{
			auto re = regex(`^[^@]+@`);
			auto m = matchFirst(arg, re);
			if (!m)
				return 0;
			return m[0].length;
		}

		int len = prefix_len(arg);
		string[] result;
		result ~= arg[0 .. len - 1];
		result ~= arg[len .. $];
		return result;
	}

	string modify_download_url(string url)
	{
		if (!url.startsWith(`https://github.com/`))
			return url;
		string orig_url = url;
		string uuid = sha1UUID("0").toString;
		uuid = format!`{%s}`(uuid);
		url = url.replace(`https://github.com/`, uuid);
		string[] parts = url.split(`/`);
		if (parts.length < 5 || parts[2] != `blob`)
			return orig_url;
		import std.algorithm : remove;

		//writeln(parts);
		parts = remove(parts, 2);
		//writeln(parts);
		return parts.join(`/`).replace(uuid, `https://raw.githubusercontent.com/`);
	}

	string url_last_part(string url)
	{
		string[] _split = url.split(`/`);
		if (_split.length == 0)
			return ``;
		return _split[_split.length - 1];
	}

	string url = null;
	if (path.startsWith(`http://`) || path.startsWith(`https://`))
	{
		url = modify_download_url(path);
		path = url_last_part(url);
	}
	else if (path.canFind("@"))
	{
		string[] split = split_donwload_path(path);
		path = split[0];
		//writeln(`new_path=`, path);
		url = modify_download_url(split[1]);
		//writeln(`url=`, url);
	}
	string abs_path;
	switch (path)
	{
	case `.`, `./`, `.\`:
		abs_path = g_context.dirName;
		break;
	default:
		if (path.startsWith(`./`) || path.startsWith(`.\`))
			path = path[2 .. $];
		abs_path = absolutePath(path, g_context.dirName).replace(`\`, `/`);
		break;
	}
	if (url !is null)
	{
		import std.net.curl : byChunkAsync;

		writefln(`Donwloading %s`, url);
		ubyte[] bytes;
		bytes.reserve(10240);
		foreach (chunk; byChunkAsync(url, 1024))
		{
			bytes ~= chunk;
		}
		bool up_to_date = false;
		try
		{
			File f_read = File(abs_path, "rb");
			scope (exit)
				f_read.close();
			ubyte[] bytes_read = f_read.rawRead(new ubyte[cast(size_t) f_read.size]);
			//ubyte[] bytes_read = cast(ubyte[]) read(abs_path);
			up_to_date = (bytes == bytes_read);
			//f_read.close();
		}
		catch (Exception ex)
		{
			//writeln(ex);
		}
		//writeln("bytes.length=", bytes.length);
		if (up_to_date)
		{
			writefln(`  ===> Up-to-date: %s`, abs_path);
		}
		else
		{
			mkdirRecurse(dirName(abs_path));
			File f = File(abs_path, "wb");
			f.rawWrite(bytes);
			f.close();
			writefln(`  ===> Writing to: %s`, abs_path);
		}
	}
	return abs_path;
}

private string[] expand_wild_cards(string path)
{
	string[] result;
	if (!path.canFind('*') && !path.canFind('?') && !path.canFind('{') && !path.canFind('}'))
	{
		result ~= path;
		return result;
	}
	try
	{
		auto files = dirEntries(dirName(path), baseName(path), SpanMode.shallow);
		foreach (file; files)
		{
			string file_name = file.name.replace(`\`, `/`);
			if (file_name.startsWith(`./`) && !path.startsWith(`./`) && !path.startsWith(`.\`))
				file_name = file_name[2 .. $];
			result ~= file_name;
		}
	}
	catch (Exception ex)
	{
	}
	return result;
}

private int emake_run_command(string[] cmdline)
{
	/+
	auto pipes = pipeProcess(dub_cmdline, Redirect.stdout | Redirect.stderrToStdout);
	foreach (line; pipes.stdout.byLine)
	{
		import std.stdio : stdout;

		writeln(line);
		stdout.flush();
	}
	int rc = wait(pipes.pid);
	return rc;
	+/
	auto pid = spawnProcess(cmdline);
	return wait(pid);
}

void rewite_dub_json(JSONValue* jsonObj, ref JSONValue*[] path_array_list, string prop_name)
{
	JSONValue* get_object_array_member(JSONValue* jsonObj, string key)
	{
		JSONValue* member = cast(JSONValue*)(key in (*jsonObj));
		if (member.type() != JSON_TYPE.ARRAY && member.type() != JSON_TYPE.STRING)
			return null;
		return member;
	}

	if (jsonObj.type == JSON_TYPE.ARRAY)
	{
		for (int i = 0; i < jsonObj.array.length; i++)
		{
			rewite_dub_json(&jsonObj.array[i], path_array_list, prop_name);
		}

	}
	else if (jsonObj.type == JSON_TYPE.OBJECT)
	{
		static string[] list = ["[root]", "configurations", "subPackages"];
		if (list.canFind(prop_name)) //(("name" in jsonObj.object) || ("targetType" in jsonObj.object))
		{
			if (!("targetPath" in jsonObj.object))
			{
				//jsonObj.object["targetPath"] = `.`;
				jsonObj.object["targetPath"] = getcwd();
			}
			if ("dependencies" in jsonObj.object)
			{
				JSONValue* dep = cast(JSONValue*)("dependencies" in jsonObj.object);
				//jsonObj.object["targetPath"] = getcwd();
				foreach (key; (*dep).object.keys())
				{
					JSONValue* member = key in (*dep).object;
					if ((*member).type == JSON_TYPE.OBJECT)
					{
						JSONValue* path = cast(JSONValue*)("path" in (*member).object);
						if (path && (*path).type == JSON_TYPE.STRING)
						{
							path_array_list ~= path;
						}
					}
				}
			}
		}
		JSONValue*[] result;
		foreach (key; jsonObj.object.keys())
		{
			JSONValue* member = key in jsonObj.object;
			rewite_dub_json(member, path_array_list, key);
			if (g_context.path_keyword_list.canFind(key.split("-")[0]))
			{
				JSONValue* array = get_object_array_member(jsonObj, key);
				if (array)
					path_array_list ~= array;
			}
		}
	}
	else
	{
	}
}

private string my_json_pprint(ref JSONValue jsonObj)
{
	// dfmt off
	int[string] dict = [
		"name" : 1, "description " : 2, "homepage" : 3, "authors" : 4, "copyright": 5,
		"license" : 6, "targetName" : 7, "targetType" : 8, "targetPath" : 9,
		"workingDirectory" : 10, "dependencies" : 11, "subConfigurations" : 12, "versions" : 13,
		"debugVersions" : 14, "stringImportPaths" : 15, "importPaths" : 16, "sourcePaths" : 17,
		"mainSourceFile" : 18, "sourceFiles" : 19, "excludedSourceFiles" : 20, "libs" : 21,
		"subPackages" : 0, "configurations" : 0, "buildTypes" : 0, "ddoxFilterArgs" : 0,
		"systemDependencies" : 0, "buildRequirements" : 0, "buildOptions" : 0, "copyFiles" : 0,
		"preGenerateCommands" : 0, "postGenerateCommands" : 0, "preBuildCommands" : 0,
		"postBuildCommands" : 0, "dflags" : 0, "lflags" : 0
	];
	// dfmt on
	void my_json_pprint_helper(JSONValue* jsonObj, string uuid)
	{
		// https://code.dlang.org/package-format?lang=json
		JSON_TYPE type = jsonObj.type;
		if (type == JSON_TYPE.ARRAY)
		{
			for (int i = 0; i < jsonObj.array.length; i++)
				my_json_pprint_helper(&jsonObj.array[i], uuid);
		}
		else if (type == JSON_TYPE.OBJECT)
		{
			string[] keys = jsonObj.object.keys;
			foreach (key; keys)
			{
				my_json_pprint_helper(&jsonObj.object[key], uuid);
				int* found = key.split("-")[0] in dict;
				if (found && (*found) != 0)
				{
					jsonObj.object[format!`<%05d-%s>`(*found, uuid) ~ key] = jsonObj.object[key];
					jsonObj.object.remove(key);
				}
			}
		}
	}

	JSONValue jsonCopy = jsonObj;
	string uuid = sha1UUID("edub").toString;
	my_json_pprint_helper(&jsonCopy, uuid);
	string result = jsonCopy.toPrettyString(JSONOptions.doNotEscapeSlashes);
	auto re = regex(format!`<(\d)+-%s>`(uuid), "g");
	result = replaceAll(result, re, "");
	return result;
}

private int handle_exe_output(string ext, string[] args)
{
	string pop(ref string[] list)
	{
		if (list.length == 0)
			return "";
		string result = list[0];
		list = list[1 .. $];
		return result;
	}

	string normalize_path(string path)
	{
		return path.replace(`\`, `/`);
	}

	string arg_strip_prefix(string arg)
	{
		int arg_prefix_len(string arg)
		{
			auto re = regex(`^[^=]+=`);
			auto m = matchFirst(arg, re);
			if (!m)
				return 0;
			return m[0].length;
		}

		return arg[arg_prefix_len(arg) .. $];
	}

	writeln(`handle_exe_output:`, args);
    stdout.flush();
	string command = pop(args);
	//writefln(`handle_exe_output: %s %s %s`, g_context.fileName, command, args);
	switch (command)
	{
	case `run`:
	case `build`:
	case `init`:
	case `test`:
	case `debug`:
		break;
	default:
		writefln(`Invalid command "%s".`, command);
		exit(1);
	}
	//int[string] dummyAlist;
	JSONValue jsonObj = parseJSON("{}"); //dummyAlist;
	//jsonObj["name"] = g_context.baseName.toLower;
	jsonObj["name"] = g_context.fileName.replace(`.`, `-`);
	jsonObj["targetName"] = g_context.baseName;
	switch (ext)
	{
	case ".exe":
		jsonObj["targetType"] = "executable";
		break;
	case ".dll":
		jsonObj["targetType"] = "dynamicLibrary";
		break;
	case ".lib":
		jsonObj["targetType"] = "library";
		break;
	default:
		break;
	}
	string[] dub_opts;
	string arch = "";
	string main_source;
	string[] source_files;
	string[] source_dirs;
	string[] include_dirs;
	string[] data_dirs;
	string[] libs;
	string[] defines;
	string[] debug_defines;
	string[] cflags;
	string[] lflags;
	string[] run_args;
	struct _PackageSpec
	{
		string _name;
		string _version;
		string _sub_config;
	}

	_PackageSpec[] packages;

	string uuid = sha1UUID("#").toString;
	uuid = format!`{%s}`(uuid);
	while (args.length)
	{
		string arg = pop(args).strip;
		//writefln(`arg="%s"`, arg);
		if (arg == "--")
		{
			run_args = args;
			break;
		}
		else if (arg.startsWith(`[`))
		{
			arg = arg.replace("{#}", uuid);
			//auto re = regex(`^\[([^:]+)(:[^:]*)?(:[^:]*)?\]$`);
			auto re = regex(`^\[([^#]+)(#[^#]*)?(#[^#]*)?\]$`);
			auto m = matchFirst(arg, re);
			if (m)
			{
				//writeln(`match!`);
				//writefln(`match="%s" "%s" "%s"`, m[1], m[2], m[3]);
				string m1 = m[1];
				string m2 = m[2];
				string m3 = m[3];
				_PackageSpec spec;
				spec._name = m1.replace(uuid, `#`);
				if (m2.startsWith(`#`))
					m2 = m2[1 .. $];
				m2 = m2.strip;
				spec._version = m2.empty ? "~master" : m2.replace(uuid, `#`);
				if (m3.startsWith(`#`))
					m3 = m3[1 .. $];
				m3 = m3.strip;
				spec._sub_config = m3.empty ? "" : m3.replace(uuid, `#`);
				packages ~= spec;
				//writeln("match end!");
			}
		}
		else if (arg.startsWith("-"))
		{
			if (arg == "--cleanup")
			{
				g_context.opt_cleanup = true;
			}
			else
			{
				dub_opts ~= arg;
			}
		}
		else if (arg.startsWith(`arch=`))
		{
			arch = arg_strip_prefix(arg);
		}
		else if (arg.startsWith(`main=`))
		{
			main_source = arg_strip_prefix(arg);
		}
		else if (arg.startsWith(`source=`) || arg.startsWith(`src=`))
		{
			source_dirs ~= normalize_path(arg_strip_prefix(arg));
		}
		else if (arg.startsWith(`cflag=`))
		{
			cflags ~= arg_strip_prefix(arg);
		}
		else if (arg.startsWith(`lflag=`))
		{
			lflags ~= arg_strip_prefix(arg);
		}
		else if (arg.startsWith(`data=`))
		{
			data_dirs ~= normalize_path(arg_strip_prefix(arg));
		}
		else if (arg.startsWith(`include=`) || arg.startsWith(`inc=`))
		{
			include_dirs ~= normalize_path(arg_strip_prefix(arg));
		}
		else if (arg.startsWith(`libs=`) || arg.startsWith(`lib=`))
		{
			libs ~= arg_strip_prefix(arg).split(`:`);
		}
		else if (arg.startsWith(`defines=`) || arg.startsWith(`defs=`)
				|| arg.startsWith(`define=`) || arg.startsWith(`def=`))
		{
			foreach (def; arg_strip_prefix(arg).split(`:`))
			{
				if (def.startsWith(`@`))
					debug_defines ~= def[1 .. $];
				else
					defines ~= def;
			}
		}
		else if (arg.canFind(`=`))
		{
			writefln("Unrecognized option: %s", arg);
			exit(1);
		}
		else
		{
			//source_files ~= expand_wild_cards(arg);
			source_files ~= arg;
		}
	}
	switch (arch)
	{
	case "ms32":
		dub_opts ~= "--arch=x86_mscoff";
		if (ext == ".dll")
		{
			source_files ~= "gcstub32mscoff.obj";
			source_files ~= "phobos32mscoff.lib";
		}
		break;
	case "ms64":
		dub_opts ~= "--arch=x86_64";
		if (ext == ".dll")
		{
			source_files ~= "gcstub64.obj";
			source_files ~= "phobos64.lib";
			//source_files ~= "curl.lib";
		}
		break;
		/+
	case "ms64_no_curl":
		dub_opts ~= "--arch=x86_64";
		if (ext == ".dll")
		{
			source_files ~= "gcstub64.obj";
			source_files ~= "phobos64.lib";
		}
		break;
	+/
	case "dm32":
		dub_opts ~= "--arch=x86";
		if (ext == ".dll")
		{
			source_files ~= "gcstub.obj";
			source_files ~= "phobos.lib";
			source_files ~= "snn.lib";
			source_files ~= format!`%s.def`(g_context.fileName);
			string def_path = format!`%s.def`(g_context.fullPath);
			//writefln(`dub_json_path="%s"`, dub_json_path);
			string def_text = format!`LIBRARY %s
EXETYPE NT
CODE PRELOAD DISCARDABLE
DATA PRELOAD MULTIPLE
`(g_context.fileName);
			File file0 = File(def_path, "w");
			file0.write(def_text);
			file0.close();
		}
		break;
	default:
		break;
	}
	if (main_source)
		jsonObj["mainSourceFile"] = main_source;
	if (source_files)
		jsonObj["sourceFiles"] = source_files;
	//if (source_dirs)
	jsonObj["sourcePaths"] = source_dirs;
	//if (include_dirs)
	jsonObj["importPaths"] = include_dirs;
	if (data_dirs)
		jsonObj["stringImportPaths"] = data_dirs;
	if (libs)
		jsonObj["libs"] = libs;
	if (defines)
		jsonObj["versions"] = defines;
	if (debug_defines)
		jsonObj["debugVersions"] = debug_defines;
	int sub_config_count = 0;
	if (cflags.length > 0)
	{
		jsonObj["dflags"] = parseJSON("[]");
		foreach (cflag; cflags)
		{
			jsonObj["dflags"].array ~= JSONValue(cflag);
		}
	}
	if (lflags.length > 0)
	{
		jsonObj["lflags"] = parseJSON("[]");
		foreach (lflag; lflags)
		{
			jsonObj["lflags"].array ~= JSONValue(lflag);
		}
	}
	if (packages.length > 0)
	{
		//string[string] dependencies_init;
		jsonObj["dependencies"] = parseJSON("{}"); //dependencies_init;
		foreach (ref pkg; packages)
		{
			if (!pkg._version.empty && pkg._version[0 .. 1] == "@")
			{
				jsonObj["dependencies"][pkg._name] = parseJSON("{}");
				jsonObj["dependencies"][pkg._name][`path`] = pkg._version[1 .. $];
			}
			else
			{
				jsonObj["dependencies"][pkg._name] = pkg._version;
			}
			if (!pkg._sub_config.empty)
				sub_config_count++;
		}
		if (sub_config_count > 0)
		{
			//string[string] sub_config_init;
			jsonObj["subConfigurations"] = parseJSON("{}"); //sub_config_init;
			foreach (ref pkg; packages)
			{
				if (pkg._sub_config.empty)
					continue;
				jsonObj["subConfigurations"][pkg._name] = pkg._sub_config;
			}
		}
	}
	//stringImportPaths
	string json = my_json_pprint(jsonObj);
	//writeln(json);
	//writeln(packages);
	string dub_json_path = format!`%s.json`(g_context.fullPath);
	//writefln(`dub_json_path="%s"`, dub_json_path);
	File file1 = File(dub_json_path, "w");
	file1.write(json);
	file1.close();
	if (command == "init")
		return 0;
	string[] new_args = ["edub.exe", dub_json_path, command];
	new_args ~= dub_opts;
	if (command == `run` && run_args.length > 0)
	{
		new_args ~= `--`;
		new_args ~= run_args;
	}
	return main(new_args);
}

void collect_compile_units(JSONValue* jsonObj, ref JSONValue*[] unit_list, string prop_name)
{
	if (jsonObj.type == JSON_TYPE.ARRAY)
	{
		for (int i = 0; i < jsonObj.array.length; i++)
		{
			collect_compile_units(&jsonObj.array[i], unit_list, prop_name);
		}

	}
	else if (jsonObj.type == JSON_TYPE.OBJECT)
	{
		static string[] list = ["[root]", "configurations", "subPackages"];
		if (list.canFind(prop_name)) //(("name" in jsonObj.object) || ("targetType" in jsonObj.object))
		{
			unit_list ~= jsonObj;
		}
		foreach (key; jsonObj.object.keys())
		{
			JSONValue* member = key in jsonObj.object;
			collect_compile_units(member, unit_list, key);
		}
	}
}

int main(string[] args)
{
	//writeln(modify_download_url("https://github.com/apache/thrift/blob/master/CHANGES")); exit(0);
	//writeln(args.length);
	//writeln(`args=`, args);
	if (args.length < 2)
	{
		writefln("Usage: edub2 PROJECT.json [build/run]");
		return 1;
	}
	g_context.fullPath = absolutePath(args[1], getcwd()).replace(`\`, `/`);
	//writefln(`g_context.fullPath=%s`, g_context.fullPath);
	g_context.dirName = dirName(g_context.fullPath);
	//writefln(`g_context.dirName=%s`, g_context.dirName);
	g_context.fileName = baseName(g_context.fullPath);
	//writefln(`g_context.fileName=%s`, g_context.fileName);
	g_context.extension = extension(g_context.fileName); //.toLower;
	//writefln(`g_context.extension=%s`, g_context.extension);
	g_context.baseName = baseName(g_context.fileName, g_context.extension);
	g_context.basePath = g_context.dirName ~ `/` ~ g_context.baseName;
	//writefln(`g_context.basePath=%s`, g_context.basePath);
	switch (g_context.extension.toLower)
	{
	case ".exe":
	case ".dll":
	case ".lib":
		return handle_exe_output(g_context.extension.toLower, args[2 .. $]);
	case ".json":
		break;
	default:
		assert(0);
		break;
	}
	string folder_name = format!"%s/%s.bin"(getcwd(), g_context.baseName).replace(`\`, `/`);

	auto jsonText = cast(char[]) read(g_context.fullPath);
	//writeln(jsonText);
	auto jsonObj = parseJSON(jsonText);

	string make_relative_to_folder(string path)
	{
		return relativePath(path, folder_name).replace(`\`, `/`);
	}
	/+
	JSONValue*[] unit_list;
	collect_compile_units(&jsonObj, unit_list, `[root]`);
	writefln(`unit_list.length=%d`, unit_list.length);
	foreach (unit; unit_list)
	{
		//unit.object["dummy"] = 1234;
		string[] source_path_list;
		string[] found_source_list;
		JSONValue* source_paths = cast(JSONValue*)("sourcePaths" in *unit);
		if (source_paths && source_paths.type == JSON_TYPE.ARRAY)
		{
			writeln("found!");
		}
		else
		{
			writeln("not found!");
			source_path_list = [`source`, `src`];
			unit.object["sourcePaths"] = parseJSON("[]");
			unit.object["sourcePaths"].array ~= JSONValue(`source`);
			unit.object["sourcePaths"].array ~= JSONValue(`src`);
		}
	}
	+/
	/+
	JSONValue*[] unit_list;
	collect_compile_units(&jsonObj, unit_list, `[root]`);
	writefln(`unit_list.length=%d`, unit_list.length);
	foreach (unit; unit_list)
	{
		//unit.object["dummy"] = 1234;
		string[] source_path_list;
		string[] found_source_list;
		JSONValue* source_paths = cast(JSONValue*)("sourcePaths" in *unit);
		if (source_paths && source_paths.type == JSON_TYPE.ARRAY)
		{
			writeln("found!");
			foreach (ref source_path; source_paths.array)
			{
				if (source_path.type != JSON_TYPE.STRING)
					continue;
				source_path_list ~= source_path.str;
			}
		}
		else
		{
			source_path_list = [`source`, `src`];
		}
		foreach (source_path; source_path_list)
		{
			try
			{
				string source_path_abs = make_abs_path(source_path);
				auto files = dirEntries(source_path_abs, "*.d", SpanMode.breadth);
				foreach (file; files)
				{
					string file_name = file.name.replace(`\`, `/`);
					if (file_name.startsWith(`./`))
						file_name = file_name[2 .. $];
					if (found_source_list.canFind(file_name)) continue;
					found_source_list ~= file_name;
					writeln(file_name);
				}
			}
			catch (Exception ex)
			{
			}
		}
		writeln(`found_source_list=`, found_source_list);
		const string[] _empty_array;
		if (found_source_list)
		{
			//writeln(`(A0)`);
			JSONValue* source_files = cast(JSONValue*)("sourceFiles" in *unit);
			if (!source_files || source_files.type != JSON_TYPE.ARRAY)
			{
				//writeln(`(A)`);
				unit.object["sourceFiles"] = _empty_array;
				source_files = cast(JSONValue*)("sourceFiles" in *unit);
				assert(source_files);
			}
			foreach (found_source; found_source_list)
			{
				//writeln(`(B)`);
				source_files.array ~= JSONValue(found_source);
			}
		}
		//writeln(`(C)`);
		unit.object["sourcePaths"] = _empty_array;
		//writeln(`(D)`);
	}
	//exit(0);
	+/
	JSONValue*[] path_array_list;
	rewite_dub_json(&jsonObj, path_array_list, `[root]`);
	//writeln(path_array_list.length);
	foreach (JSONValue* path_array; path_array_list)
	{
		if (path_array.type == JSON_TYPE.STRING)
		{
			path_array.str = make_relative_to_folder(make_abs_path(path_array.str));
			//path_array.str = make_abs_path(path_array.str);
			continue;
		}
		assert(path_array.type == JSON_TYPE.ARRAY);
		//writeln(path_array.array.length);
		JSONValue[] new_array;
		string[] check_array;
		for (int i = 0; i < path_array.array.length; i++)
		{
			auto val = path_array.array[i];
			if (val.type() != JSON_TYPE.STRING)
			{
				new_array ~= val;
				continue;
			}
			string abs_path = make_abs_path(val.str);
			foreach (real_path; expand_wild_cards(abs_path))
			{
				if (check_array.canFind(real_path))
					continue;
				check_array ~= real_path;
				new_array ~= JSONValue(make_relative_to_folder(real_path));
				//new_array ~= JSONValue(real_path);
			}
		}
		path_array.array.length = 0;
		path_array.array ~= new_array;
	}

	auto jsonText2 = my_json_pprint(jsonObj);
	//writeln(jsonText2);
	//string folder_name = format!"%s.bin"(g_context.basePath);
	//string folder_name = format!"%s/%s.bin"(getcwd(), g_context.baseName).replace(`\`, `/`);
	//writeln(`folder_name=`, folder_name); //exit(0);
	//writeln(folder_name);
	mkdirRecurse(folder_name);
	try
	{
		auto currentTime = Clock.currTime();
		setTimes(folder_name, currentTime, currentTime);
	}
	catch (Exception ex)
	{
	}
	string dub_json_path = folder_name ~ "/dub.json";
	//writeln(dub_json_path);
	File file1 = File(dub_json_path, "w");
	file1.write(jsonText2);
	file1.close();
	string[] dub_cmdline;
	string[] run_args;
	dub_cmdline ~= "dub";
	for (int i = 2; i < args.length; i++)
	{
		if (args[i] == `--`)
		{
			run_args = args[i + 1 .. $];
			break;
		}
		else if (args[i] == "--cleanup")
		{
			g_context.opt_cleanup = true;
		}
		else
		{
			dub_cmdline ~= args[i];
		}
	}
	if (dub_cmdline.canFind("debug"))
	{
		writeln(jsonText2);
        stdout.flush();
		exit(0);
	}
	if (dub_cmdline.length >= 2 && dub_cmdline[1] == "generate")
	{
		chdir(folder_name);
		writeln(dub_cmdline);
        stdout.flush();
		int rc = emake_run_command(dub_cmdline);
		return rc;
	}
	dub_cmdline ~= format!`--root=%s`(folder_name);
	if (run_args)
	{
		dub_cmdline ~= `--`;
		dub_cmdline ~= run_args;
	}
	writeln(dub_cmdline);
    stdout.flush();
	int rc = emake_run_command(dub_cmdline);
	if (rc == 0)
	{
		if (g_context.opt_cleanup)
		{
			try
			{
				rmdirRecurse(folder_name);
			}
			catch (Exception ex)
			{
			}
		}
	}
	return rc;
}
