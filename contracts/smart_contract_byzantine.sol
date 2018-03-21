pragma solidity ^0.4.0;
contract Estimation {

int public mean;
int public count;
int public threshold = 100000;
int public m2;
int W_n;

mapping(address => int) weights;


event consensusReached(uint c);

 function sqrt(int x) constant  returns (int y) {
   int z = (x + 1) / 2;
   y = x;
   while (z < y) {
     y = z;
     z = (x / z + z) / 2;
   }
 }

function weightTest() payable {

  weights[msg.sender] = 1;
  int old_mean = mean;
  int w_n = weights[msg.sender];
  int x_n = int(msg.value);
  int delta = x_n - mean;
  W_n = W_n + w_n;
  mean = mean + (w_n / W_n) * delta;

}

 function vote() payable {

   int w_n = weights[msg.sender];
   int x_n = int(msg.value);
   count = count + 1;
   int delta = x_n - mean;

   // Weighted average
   // Remember that we have ints only

   int old_mean = mean;
   mean = mean + (w_n / W_n) * delta;
   W_n = W_n + w_n;

   int S_n_old = S_n;
   int S_n = S_n_old + w_n * (x_n - old_mean) * (x_n - mean);
   int sigma_n = sqrt(S_n / W_n);

   // Handle consensus
   if (count < 2) {
     consensusReached(1);
   } else {

    int myvar = m2 / (count - 1);
    int acc = myvar / count;
    int se = sqrt(acc);

    if (se < threshold && count > 10) {
        consensusReached(2);
      } else {
        consensusReached(1);
      }
    }
 }

 function getMean() constant returns (int) {
   return mean;
 }

 function getCount() constant returns (int) {
   return count;
 }

 function calcSE() constant returns (int) {
  int myvar = m2 / (count - 1);
  int acc = myvar / count;
  int se = sqrt(acc);

  return se;
 }

 function checkStop() constant returns (int) {

   if (count < 2) {
     return 2;
   }

   int myvar = m2 / (count - 1);
   int acc = myvar / count;
   int se = sqrt(acc);

   if (se < threshold) {
     return 1;
   } else {
     return 2;
   }
 }
}
