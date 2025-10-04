import { describe, it, expect, beforeEach } from "vitest";
import { stringAsciiCV, uintCV } from "@stacks/transactions";

const ERR_NOT_AUTHORIZED = 100;
const ERR_PROPOSAL_NOT_FOUND = 101;
const ERR_ALREADY_VOTED = 102;
const ERR_PROPOSAL_ENDED = 103;
const ERR_INSUFFICIENT_STAKE = 104;
const ERR_INVALID_TITLE = 105;
const ERR_INVALID_DESCRIPTION = 106;
const ERR_INVALID_DURATION = 107;
const ERR_INVALID_PROPOSAL_TYPE = 108;
const ERR_PROPOSAL_ACTIVE = 109;
const ERR_NO_QUORUM = 110;
const ERR_ALREADY_EXECUTED = 111;
const ERR_INVALID_STAKE_AMOUNT = 112;
const ERR_MEMBER_NOT_FOUND = 113;
const ERR_INVALID_THRESHOLD = 118;
const ERR_MAX_PROPOSALS_EXCEEDED = 119;

interface Proposal {
  creator: string;
  title: string;
  description: string;
  startBlock: number;
  endBlock: number;
  votesFor: number;
  votesAgainst: number;
  executed: boolean;
  proposalType: string;
  param?: number;
}

interface Vote {
  vote: boolean;
}

interface Result<T> {
  ok: boolean;
  value: T;
}

class DaoGovernanceMock {
  state: {
    proposalCount: number;
    minStake: number;
    maxProposals: number;
    quorumThreshold: number;
    treasury: string;
    proposals: Map<number, Proposal>;
    votes: Map<string, Vote>;
    memberStakes: Map<string, number>;
    totalVotes: Map<number, number>;
  } = {
    proposalCount: 0,
    minStake: 1000,
    maxProposals: 1000,
    quorumThreshold: 50,
    treasury: "ST1TEST",
    proposals: new Map(),
    votes: new Map(),
    memberStakes: new Map(),
    totalVotes: new Map(),
  };
  blockHeight: number = 0;
  caller: string = "ST1TEST";
  balances: Map<string, number> = new Map([["ST1TEST", 10000], ["ST2TEST", 5000]]);
  transfers: Array<{ amount: number; from: string; to: string }> = [];

  reset() {
    this.state = {
      proposalCount: 0,
      minStake: 1000,
      maxProposals: 1000,
      quorumThreshold: 50,
      treasury: "ST1TEST",
      proposals: new Map(),
      votes: new Map(),
      memberStakes: new Map(),
      totalVotes: new Map(),
    };
    this.blockHeight = 0;
    this.caller = "ST1TEST";
    this.balances = new Map([["ST1TEST", 10000], ["ST2TEST", 5000]]);
    this.transfers = [];
  }

  getProposal(id: number): Result<Proposal | null> {
    return { ok: true, value: this.state.proposals.get(id) || null };
  }

  getVote(proposalId: number, voter: string): Result<Vote | null> {
    return { ok: true, value: this.state.votes.get(`${proposalId}-${voter}`) || null };
  }

  getMemberStake(member: string): Result<number> {
    return { ok: true, value: this.state.memberStakes.get(member) || 0 };
  }

  getTotalVotes(id: number): Result<number> {
    return { ok: true, value: this.state.totalVotes.get(id) || 0 };
  }

  joinDao(stakeAmount: number): Result<boolean> {
    if (stakeAmount < this.state.minStake) return { ok: false, value: false };
    if ((this.balances.get(this.caller) || 0) < stakeAmount) return { ok: false, value: false };
    this.balances.set(this.caller, (this.balances.get(this.caller) || 0) - stakeAmount);
    this.transfers.push({ amount: stakeAmount, from: this.caller, to: this.caller });
    const currentStake = this.state.memberStakes.get(this.caller) || 0;
    this.state.memberStakes.set(this.caller, currentStake + stakeAmount);
    return { ok: true, value: true };
  }

  leaveDao(): Result<boolean> {
    const stake = this.state.memberStakes.get(this.caller) || 0;
    if (stake === 0) return { ok: false, value: false };
    this.balances.set(this.caller, (this.balances.get(this.caller) || 0) + stake);
    this.transfers.push({ amount: stake, from: this.caller, to: this.caller });
    this.state.memberStakes.delete(this.caller);
    return { ok: true, value: true };
  }

  createProposal(title: string, description: string, duration: number, ptype: string, param?: number): Result<number> {
    if (this.state.proposalCount >= this.state.maxProposals) return { ok: false, value: ERR_MAX_PROPOSALS_EXCEEDED };
    if (title.length === 0 || title.length > 100) return { ok: false, value: ERR_INVALID_TITLE };
    if (description.length === 0 || description.length > 500) return { ok: false, value: ERR_INVALID_DESCRIPTION };
    if (duration < 100 || duration > 10000) return { ok: false, value: ERR_INVALID_DURATION };
    if (!["content-approval", "royalty-rate", "platform-upgrade", "quorum-change"].includes(ptype)) return { ok: false, value: ERR_INVALID_PROPOSAL_TYPE };
    if ((this.state.memberStakes.get(this.caller) || 0) < this.state.minStake) return { ok: false, value: ERR_INSUFFICIENT_STAKE };
    const id = this.state.proposalCount + 1;
    this.state.proposals.set(id, {
      creator: this.caller,
      title,
      description,
      startBlock: this.blockHeight,
      endBlock: this.blockHeight + duration,
      votesFor: 0,
      votesAgainst: 0,
      executed: false,
      proposalType: ptype,
      param,
    });
    this.state.totalVotes.set(id, 0);
    this.state.proposalCount = id;
    return { ok: true, value: id };
  }

  vote(proposalId: number, voteFor: boolean): Result<boolean> {
    const proposal = this.state.proposals.get(proposalId);
    if (!proposal) return { ok: false, value: false };
    const stake = this.state.memberStakes.get(this.caller) || 0;
    if (stake < this.state.minStake) return { ok: false, value: ERR_INSUFFICIENT_STAKE };
    if (this.blockHeight > proposal.endBlock) return { ok: false, value: ERR_PROPOSAL_ENDED };
    if (this.state.votes.has(`${proposalId}-${this.caller}`)) return { ok: false, value: ERR_ALREADY_VOTED };
    this.state.votes.set(`${proposalId}-${this.caller}`, { vote: voteFor });
    const currentVotes = this.state.totalVotes.get(proposalId) || 0;
    this.state.totalVotes.set(proposalId, currentVotes + stake);
    if (voteFor) {
      this.state.proposals.set(proposalId, { ...proposal, votesFor: proposal.votesFor + stake });
    } else {
      this.state.proposals.set(proposalId, { ...proposal, votesAgainst: proposal.votesAgainst + stake });
    }
    return { ok: true, value: true };
  }

  getProposalCount(): Result<number> {
    return { ok: true, value: this.state.proposalCount };
  }

  isMember(who: string): Result<boolean> {
    return { ok: true, value: (this.state.memberStakes.get(who) || 0) > 0 };
  }
}

describe("DaoGovernance", () => {
  let contract: DaoGovernanceMock;

  beforeEach(() => {
    contract = new DaoGovernanceMock();
    contract.reset();
  });

  it("creates a proposal successfully", () => {
    contract.joinDao(1000);
    const result = contract.createProposal("Test Proposal", "Description", 1000, "content-approval");
    expect(result.ok).toBe(true);
    expect(result.value).toBe(1);
    const proposal = contract.getProposal(1).value;
    expect(proposal?.title).toBe("Test Proposal");
    expect(proposal?.description).toBe("Description");
    expect(proposal?.proposalType).toBe("content-approval");
  });

  it("rejects proposal with invalid title", () => {
    contract.joinDao(1000);
    const result = contract.createProposal("", "Description", 1000, "content-approval");
    expect(result.ok).toBe(false);
    expect(result.value).toBe(ERR_INVALID_TITLE);
  });

  it("rejects proposal with insufficient stake", () => {
    const result = contract.createProposal("Test Proposal", "Description", 1000, "content-approval");
    expect(result.ok).toBe(false);
    expect(result.value).toBe(ERR_INSUFFICIENT_STAKE);
  });

  it("allows voting on a proposal", () => {
    contract.joinDao(1000);
    contract.createProposal("Test Proposal", "Description", 1000, "content-approval");
    const result = contract.vote(1, true);
    expect(result.ok).toBe(true);
    const proposal = contract.getProposal(1).value;
    expect(proposal?.votesFor).toBe(1000);
    expect(contract.getTotalVotes(1).value).toBe(1000);
  });

  it("rejects duplicate voting", () => {
    contract.joinDao(1000);
    contract.createProposal("Test Proposal", "Description", 1000, "content-approval");
    contract.vote(1, true);
    const result = contract.vote(1, false);
    expect(result.ok).toBe(false);
    expect(result.value).toBe(ERR_ALREADY_VOTED);
  });

  it("rejects voting after proposal end", () => {
    contract.joinDao(1000);
    contract.createProposal("Test Proposal", "Description", 1000, "content-approval");
    contract.blockHeight = 2000;
    const result = contract.vote(1, true);
    expect(result.ok).toBe(false);
    expect(result.value).toBe(ERR_PROPOSAL_ENDED);
  });

  it("allows joining DAO with sufficient stake", () => {
    const result = contract.joinDao(1000);
    expect(result.ok).toBe(true);
    expect(contract.getMemberStake("ST1TEST").value).toBe(1000);
    expect(contract.balances.get("ST1TEST")).toBe(9000);
  });

  it("allows leaving DAO", () => {
    contract.joinDao(1000);
    const result = contract.leaveDao();
    expect(result.ok).toBe(true);
    expect(contract.getMemberStake("ST1TEST").value).toBe(0);
    expect(contract.balances.get("ST1TEST")).toBe(10000);
  });

  it("returns correct proposal count", () => {
    contract.joinDao(1000);
    contract.createProposal("Test Proposal", "Description", 1000, "content-approval");
    contract.createProposal("Test Proposal 2", "Description 2", 1000, "royalty-rate", 20);
    const result = contract.getProposalCount();
    expect(result.ok).toBe(true);
    expect(result.value).toBe(2);
  });

  it("checks member status correctly", () => {
    contract.joinDao(1000);
    const result = contract.isMember("ST1TEST");
    expect(result.ok).toBe(true);
    expect(result.value).toBe(true);
    const result2 = contract.isMember("ST2TEST");
    expect(result2.ok).toBe(true);
    expect(result2.value).toBe(false);
  });
});