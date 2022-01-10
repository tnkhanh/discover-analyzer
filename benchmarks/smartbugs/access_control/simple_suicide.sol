/*
 * @source: https://github.com/SmartContractSecurity/SWC-registry/blob/master/test_cases/unprotected_critical_functions/simple_suicide.sol
 * @author: -
 * @vulnerable_at_lines: 12,13
 */

//added prgma version
// pragma solidity ^0.4.0;
pragma solidity ^0.8.11;

contract SimpleSuicide {
  // <yes> <report> ACCESS_CONTROL
  // function sudicideAnyone() {
  function sudicideAnyone() public {  // TRUNG: updated to Solidity 0.8.11
    selfdestruct(msg.sender);
  }

}
