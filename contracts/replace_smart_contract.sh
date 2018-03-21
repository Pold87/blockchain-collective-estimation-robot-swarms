rm Estimation.bin Estimation.abi

solc --abi --bin -o . smart_contract.sol

scp Estimation.bin vstrobel@majorana.ulb.ac.be:/home/vstrobel/Documents/col_estimation/controllers/epuck_environment_classification/data.txt

scp Estimation.abi vstrobel@majorana.ulb.ac.be:/home/vstrobel/Documents/col_estimation/controllers/epuck_environment_classification/interface.txt
