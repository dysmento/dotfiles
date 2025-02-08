function gup
    set REPO $argv
    set MAIN (git -C $REPO remote show origin | grep 'HEAD branch' | cut -d' ' -f5)
    echo (basename (git -C $REPO rev-parse --show-toplevel)):$MAIN
    git -C $REPO checkout --detach >/dev/null 2>&1
    git -C $REPO fetch origin $MAIN:$MAIN
end
