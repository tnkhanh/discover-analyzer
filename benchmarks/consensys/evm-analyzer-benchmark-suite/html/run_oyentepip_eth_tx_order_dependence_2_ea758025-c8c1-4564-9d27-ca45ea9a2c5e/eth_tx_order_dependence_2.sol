pragma solidity ^0.4.16;

contract Benchmark {
    uint256 public reward;
    address owner;

    function Benchmark() {
        owner = msg.sender;
    }

    function setReward() public payable {
        require(msg.sender == owner);

        owner.transfer(reward); //refund previous reward
        reward = msg.value;
    }
}