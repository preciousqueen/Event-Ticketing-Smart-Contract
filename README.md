# Event Ticketing Smart Contract

A comprehensive Clarity smart contract for decentralized event ticketing on the Stacks blockchain. This contract provides a complete ticketing ecosystem with event management, secure ticket sales, validation, transfers, and refund capabilities.

## Features

### Core Functionality
- **Event Creation**: Organizers can create events with detailed information, pricing, and capacity
- **Ticket Sales**: Users can purchase multiple tickets with automatic payment processing
- **Ticket Validation**: Secure ticket usage system preventing double-spending
- **Ticket Transfers**: Transfer ticket ownership between users
- **Refund System**: Optional refunds when enabled by event organizers
- **Revenue Management**: Automatic platform fee distribution

### Security Features
- Input validation for all user-provided data
- Authorization checks for sensitive operations
- Protection against double-spending and unauthorized access
- Row-level security for ticket ownership

## Contract Structure

### Data Maps
- `events`: Stores event information (organizer, details, pricing, capacity)
- `tickets`: Individual ticket records with ownership and usage status
- `user-tickets`: Tracks ticket counts per user per event

### Key Constants
- Platform fee rate: 2.5% (250 basis points)
- Maximum tickets per transaction: 20
- Contract owner privileges for fee management

## Functions

### Read-Only Functions
- `get-event(event-id)`: Retrieve event details
- `get-ticket(ticket-id)`: Get ticket information
- `get-user-ticket-count(user, event-id)`: Check user's ticket count
- `get-platform-fee(amount)`: Calculate platform fee
- `is-event-active(event-id)`: Check if event is active and available

### Public Functions

#### Event Management
```clarity
(create-event name description venue event-date ticket-price total-tickets refund-enabled)
