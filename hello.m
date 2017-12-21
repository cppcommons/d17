#import <stdio.h>
#import <objc/runtime.h>
#import <objc/Object.h>

@interface Super { @public id isa; } @end
@implementation Super 
+(void) initialize { } 
+(Class) class { return self; }
@end

@interface Sub : Super { int array[128]; } @end
@implementation Sub @end

@interface TestClass : Super
- (void) getMessage;
@end

@implementation TestClass
- (void) getMessage {
	printf("Hello Objective-C World\n");
}
@end

int main(int argc, char *argv[]) {
	//id obj = [ TestClass alloc ];
	//id obj = [ TestClass new ];
	printf("1: 0x%08x\n", [TestClass class]);
	//id obj = class_createInstance([TestClass class], 0);
	id obj = class_createInstance([TestClass class], 4);
	printf("2: 0x%08x\n", obj);
	[ obj getMessage ];
	printf("3\n");

	return 0;
}
