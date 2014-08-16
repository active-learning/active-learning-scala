echo "dbs"
rsync -avz -e "ssh -p 22003" 143.107.183.114:/home/davi/backup-dbs/ /home/davi/wcs/ucipp/uci/  --exclude=locked --exclude=*.arff 

echo "home"
rsync -avz -e "ssh -p 22003" 143.107.183.114:/home/davi/ /home/davi/backup-servers/  --exclude=wcs --exclude=backup-dbs --exclude=sbt --exclude=wekafiles --exclude=.bash_history --exclude=.bzr.log --exclude=.ivy2 --exclude=.sbt --exclude=.Rhistory

