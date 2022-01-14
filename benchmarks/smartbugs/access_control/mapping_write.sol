/*
 * @source: https://smartcontractsecurity.github.io/SWC-registry/docs/SWC-124#mapping-writesol
 * @author: Suhabe Bugrara
 * @vulnerable_at_lines: 20
 */

// pragma solidity ^0.4.24;
pragma solidity ^0.8.11;

 //This code is derived from the Capture the Ether https://capturetheether.com/challenges/math/mapping/

 contract Map {
     // address public owner;      // TRUNG: original code
     address payable public owner; // updated to comply with Solidity 0.8.11
     uint256[] map;

     function set(uint256 key, uint256 value) public {
         if (map.length <= key) {
             // map.length = key + 1;                  // TRUNG: original code
             for (uint i = map.length; i <= key; i++)  // updated to comply with Solidity 0.8.11
                 map.push();
         }
         // <yes> <report> ACCESS_CONTROL
         map[key] = value;
     }

     function get(uint256 key) public view returns (uint256) {
         return map[key];

     }
     function withdraw() public{
         require(msg.sender == owner);
         // msg.sender.transfer(address(this).balance); // TRUNG: original code
         owner.transfer(address(this).balance);    // updated to comply with Solidity 0.8.11
     }
 }
