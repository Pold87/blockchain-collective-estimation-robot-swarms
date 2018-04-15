NOW=`date +"%d-%m-%Y"`
RACK=3
#TEMPLATEFILE=$HOME/Documents/col_estimation/start_experiment3-safe.sh
TEMPLATEFILE=$HOME/Documents/col_estimation/start_experiment3.sh

echo "nohup bash ${TEMPLATEFILE} 0 1 0 0 > nohup_experiment3-${NOW}.out 2>&1&" > last_template.txt
echo "nohup bash ${TEMPLATEFILE} 6 13 1 1 > nohup_experiment3b-${NOW}.out 2>&1&" >> last_template.txt
