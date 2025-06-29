# Villatoken - Village Market Token

A community-specific cryptocurrency built on Stacks blockchain for facilitating local trade and commerce within village communities.

## Overview

Villatoken is a fungible token smart contract that enables communities to create their own local economy with an integrated marketplace for trading goods and services. The token promotes local commerce, community engagement, and economic sustainability.

## Features

### Token Functionality
- **Fungible Token**: Standard SIP-010 compliant token
- **Minting**: Controlled minting by authorized users
- **Burning**: Token burning capability
- **Transfer**: Secure peer-to-peer transfers

### Marketplace Features
- **Item Listing**: List goods and services for sale
- **Purchase System**: Buy items using Villatokens
- **Price Management**: Update listing prices
- **Marketplace Fees**: Configurable transaction fees
- **Reputation System**: Track user buying/selling history

### Governance
- **Owner Controls**: Marketplace toggle and fee management
- **Minter Management**: Add/remove authorized minters
- **Fee Configuration**: Adjustable marketplace fees (0-10%)

## Contract Details

- **Token Name**: Villatoken
- **Symbol**: VILLA
- **Decimals**: 6
- **Initial Supply**: 1,000,000 tokens (minted to contract owner)

## Usage

### Basic Token Operations

#### Check Balance
```clarity
(contract-call? .Villatoken get-balance 'SP1ABCD...)
```

#### Transfer Tokens
```clarity
(contract-call? .Villatoken transfer u100000 tx-sender 'SP1ABCD... none)
```

#### Mint Tokens (Owner/Authorized Only)
```clarity
(contract-call? .Villatoken mint u50000 'SP1ABCD...)
```

### Marketplace Operations

#### List an Item
```clarity
(contract-call? .Villatoken list-item "Fresh Tomatoes" "Organic tomatoes from local farm" u25000 "Produce")
```

#### Purchase an Item
```clarity
(contract-call? .Villatoken purchase-item u1)
```

#### Update Item Price
```clarity
(contract-call? .Villatoken update-item-price u1 u30000)
```

#### Remove Item Listing
```clarity
(contract-call? .Villatoken remove-item-listing u1)
```

### Read-Only Functions

#### Get Item Details
```clarity
(contract-call? .Villatoken get-item-details u1)
```

#### Get User Items
```clarity
(contract-call? .Villatoken get-user-items 'SP1ABCD...)
```

#### Get User Reputation
```clarity
(contract-call? .Villatoken get-user-reputation 'SP1ABCD...)
```

## Administration

### Toggle Marketplace
```clarity
(contract-call? .Villatoken toggle-marketplace)
```

### Set Marketplace Fee (0-1000 = 0%-10%)
```clarity
(contract-call? .Villatoken set-marketplace-fee u250)
```

### Add Authorized Minter
```clarity
(contract-call? .Villatoken add-minter 'SP1ABCD...)
```

## Error Codes

- `u100`: Owner only operation
- `u101`: Not token owner
- `u102`: Insufficient balance
- `u103`: Item not found
- `u104`: Item not for sale
- `u105`: Insufficient payment
- `u106`: Invalid amount
- `u107`: Self transfer not allowed
- `u108`: Marketplace disabled
- `u109`: Invalid price
- `u110`: Item already listed

## Development

### Prerequisites
- Clarinet CLI
- Node.js and npm

### Installation
```bash
clarinet new villatoken-project
cd villatoken-project
# Copy contract files to contracts/ directory
```

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy --testnet
```

## Community Use Cases

1. **Local Farmers Market**: Farmers can list produce and accept Villatokens
2. **Service Exchange**: Community members offer services (repairs, tutoring, etc.)
3. **Artisan Goods**: Local craftspeople sell handmade items
4. **Community Events**: Token rewards for participation
5. **Micro-loans**: Community lending programs

## License

This project is open source and available under the MIT License.
