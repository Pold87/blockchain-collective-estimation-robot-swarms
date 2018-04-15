pragma solidity ^0.4.0;
contract Estimation {

  int public mean;
  int public count;
  int public threshold = THRESHOLD;
  int public m2;
  int public pub_se;
  int W_n;
  int S_n;
  int S_n_old;
  int se;  
 
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

    count = count + 1;
    
    // Initialize this sender if it's the first time it votes
    if (weights[msg.sender] == 0) {
      weights[msg.sender] = 100000000;
    } else if (count > 20) {
      // Update its quality
      //weights[msg.sender] += 100 * (se - abs(delta));
      int absDelta = abs(delta);
      //      weights[msg.sender] += (2000000 - abs(delta));
      weights[msg.sender] += -1 * (absDelta / 1000) * (absDelta / 1000) + 2000000;
    }

    // Ignore everything if the robots sensor is bad
    if (weights[msg.sender] > 0) {


      int w_n = weights[msg.sender];
      W_n = W_n + w_n;
      mean = mean + (w_n * delta) / W_n;

      if (count > 2) {
      
	S_n_old = S_n;
	S_n = S_n_old + w_n * (x_n - old_mean) * (x_n - mean);
	se = sqrt(S_n / W_n);
	pub_se = se;
      }

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
    //    int myvar = m2 / (count - 1);
    //int acc = myvar / count;
    //int se = sqrt(acc);

    return pub_se;
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
