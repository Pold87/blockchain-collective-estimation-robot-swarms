pragma solidity ^0.4.0;
contract Estimation {

int public mean;
int public count;
int public threshold = 100000;
int public m2;
int public pub_se;
int W_n;

mapping(address => int) public weights;

event consensusReached(uint c);


 function sqrt(int x) constant  returns (int y) {
   int z = (x + 1) / 2;
   y = x;
   while (z < y) {
     y = z;
     z = (x / z + z) / 2;
   }
 }

function abs(int x) returns (int y) {
    if (x < 0) {
        return -x;
    } else {
        return x;
    }
}

 function vote() payable {

    int x_n = int(msg.value);
    int old_mean = mean;
    int delta = x_n - mean;

     // Initialize this sender if it's the first time it votes
     if (weights[msg.sender] == 0) {
        weights[msg.sender] = 10000000;
     } else if (count > 2) {
    // Update its quality
        //weights[msg.sender] += 100 * (se - abs(delta));
        weights[msg.sender] += (1000000 - abs(delta)) / 10;

     }

     // Ignore everything if the robots sensor is bad
     if (weights[msg.sender] > 0) {

  count = count + 1;
  int w_n = weights[msg.sender];
  W_n = W_n + w_n;
  mean = mean + (w_n * delta) / W_n;

  int S_n_old = S_n;
  int S_n = S_n_old + w_n * (x_n - old_mean) * (x_n - mean);
  int se = sqrt(S_n / W_n);
  pub_se = se;

}

   // Handle consensus
   if (count < 2) {
     consensusReached(1);
   } else {

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
