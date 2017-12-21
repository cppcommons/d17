if [ "$HOSTNAME" == "INOT01" -o "$HOSTNAME" == "JPA313M601" ]; then
    export PATH=/cygdrive/c/Users/Public/D/dmd2/windows/bin:$PATH
    export PATH=/cygdrive/c/Users/Public/dev1/bin32:$PATH
fi
export PATH=.:$PATH

git config --global core.autoCRLF false

git config --global user.name  root
git config --global user.email root@super-computer

git config --unset-all credential.helper
git config --global --unset-all credential.helper

git config --global credential.helper manager
git config --global credential.useHttpPath true

