/*
 * @source: https://github.com/trailofbits/not-so-smart-contracts/blob/master/wrong_constructor_name/incorrect_constructor.sol
 * @author: Ben Perez
 * @vulnerable_at_lines: 20
 */

// pragma solidity ^0.4.24;
pragma solidity ^0.8.11;

contract Missing{
    // address private owner;       // TRUNG: original code
    address payable private owner;  // updated to comply with Solidity 0.8.11

    modifier onlyowner {
        require(msg.sender==owner);
        _;
    }

    // The name of the constructor should be Missing
    // Anyone can call the IamMissing once the contract is deployed
    // <yes> <report> ACCESS_CONTROL
    function IamMissing()
        public
    {
        owner = msg.sender;
    }

    // function () payable {}      // TRUNG: original code
    receive () external payable {} // updated to comply with Solidity 0.8.11

    function withdraw()
        public
        onlyowner
    {
        // owner.transfer(this.balance);       // TRUNG: original code
        owner.transfer(payable(this).balance); // updated to comply with Solidity 0.8.11
    }
}
