# Decentralized Insurance Pool Smart Contract

A Clarity smart contract that implements a decentralized insurance pool where members can contribute funds, purchase coverage, file claims, and vote on claim approvals through a democratic process.

## Overview

This contract enables a community-driven insurance model where:
- Members deposit STX tokens into a shared pool
- Users purchase insurance coverage by paying premiums
- Claims are filed against active coverage
- Pool members vote to approve or reject claims
- Approved claims are paid out from the pool balance

## Features

- **Pool Membership**: Join the insurance pool by contributing STX tokens
- **Coverage Purchase**: Buy insurance coverage with customizable premium and payout amounts
- **Claim Filing**: File claims against active coverage with descriptions
- **Democratic Voting**: Pool members vote on claim approvals (simple majority rule)
- **Automatic Payouts**: Approved claims are automatically paid from the pool
- **Coverage Management**: Deactivate coverage when no longer needed
- **Event Logging**: All major actions are logged for transparency

## Getting Started

### Prerequisites

- Stacks blockchain environment
- STX tokens for pool contributions and premiums
- Clarity smart contract deployment tools

### Deployment

Deploy the contract to the Stacks blockchain using your preferred deployment method.

## Usage Examples

### 1. Join the Insurance Pool

```clarity
;; Contribute 1000 STX to join the pool
(contract-call? .insurance-pool join-pool u1000)
