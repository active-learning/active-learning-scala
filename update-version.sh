echo $(git shortlog -s -n | cut -d'd' -f1 | head -n1) > minV
echo $(cat majV).$(cat  minV) > VERSION
cat VERSION
