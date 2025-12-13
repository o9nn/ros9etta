implement DistributedMind;

# Example: Distributed Mind
# Demonstrates distributed cognition using Inferno's 9P protocol

include "sys.m";
    sys: Sys;
include "draw.m";
include "../include/cogkernel.m";
    cogkernel: CogKernel;

DistributedMind: module {
    init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, args: list of string)
{
    sys = load Sys Sys->PATH;
    cogkernel = load CogKernel CogKernel->PATH;
    
    sys->print("\n=== Distributed Cognitive System ===\n");
    sys->print("Demonstrating distributed thinking via 9P protocol\n\n");
    
    # Initialize local cognitive kernel
    sys->print("[Node 1] Initializing local cognitive kernel...\n");
    local_sys := cogkernel->init();
    if (local_sys == nil) {
        sys->print("Error: Cannot initialize local cognitive system\n");
        return;
    }
    sys->print("  ✓ Local mind online\n");
    
    # Create local knowledge
    sys->print("\n[Node 1] Creating local knowledge...\n");
    tv1 := cogkernel->newtruthvalue(0.9, 0.8);
    local_knowledge := cogkernel->createatom(local_sys, "local_knowledge", tv1);
    sys->print("  Created: local_knowledge (0.9, 0.8)\n");
    
    # Simulate remote cognitive kernel (in real implementation, would mount via 9P)
    sys->print("\n[Node 2] Simulating remote cognitive kernel...\n");
    remote_sys := cogkernel->init();
    if (remote_sys == nil) {
        sys->print("Error: Cannot initialize remote cognitive system\n");
        return;
    }
    sys->print("  ✓ Remote mind online\n");
    
    # Create remote knowledge
    sys->print("\n[Node 2] Creating remote knowledge...\n");
    tv2 := cogkernel->newtruthvalue(0.8, 0.9);
    remote_knowledge := cogkernel->createatom(remote_sys, "remote_knowledge", tv2);
    sys->print("  Created: remote_knowledge (0.8, 0.9)\n");
    
    # Distributed reasoning
    sys->print("\n[Distributed] Combining knowledge from both nodes...\n");
    
    # Real 9P implementation would work as follows:
    # 1. Start 9P server on remote node:
    #    styxlisten -A tcp!*!9P export /cogkernel
    # 2. Mount from local node:
    #    mount -A tcp!remotehost!9P /mnt/remotemind
    # 3. Access remote atoms via filesystem:
    #    cat /mnt/remotemind/atomspace/nodes/concept_X
    # 4. File operations trigger kernel operations:
    #    - Read file = query kernel for atom
    #    - Write file = create/update atom in remote kernel
    #    - Directory listing = enumerate atoms by type
    # See ARCHITECTURE.md section "Distributed Cognition via 9P" for details
    
    # Simulate knowledge sharing
    shared_knowledge := cogkernel->createatom(local_sys, "shared_knowledge", tv1);
    sys->print("  Shared knowledge created in local node\n");
    
    # Link local and remote knowledge (conceptually)
    sys->print("\n[Distributed] Creating distributed links...\n");
    link1 := cogkernel->sys_atom_link(local_sys, local_knowledge, 
                                      shared_knowledge, "relates_to");
    sys->print("  Local -> Shared: %s\n", link1.linktype);
    
    # Distributed reasoning
    sys->print("\n[Distributed] Performing distributed reasoning...\n");
    
    # Local reasoning
    local_premises := local_knowledge :: shared_knowledge :: nil;
    local_inferences := cogkernel->deduce(local_sys, local_premises);
    sys->print("  Local inferences: %d\n", len local_inferences);
    
    # Remote reasoning
    remote_premises := remote_knowledge :: nil;
    remote_inferences := cogkernel->deduce(remote_sys, remote_premises);
    sys->print("  Remote inferences: %d\n", len remote_inferences);
    
    # Attention allocation across nodes
    sys->print("\n[Distributed] Allocating attention across network...\n");
    
    # Focus local attention
    cogkernel->sys_focus(local_sys, local_knowledge);
    local_importance := cogkernel->sys_importance(local_sys, local_knowledge);
    sys->print("  Local importance: %.1f STI\n", local_importance);
    
    # Focus remote attention
    cogkernel->sys_focus(remote_sys, remote_knowledge);
    remote_importance := cogkernel->sys_importance(remote_sys, remote_knowledge);
    sys->print("  Remote importance: %.1f STI\n", remote_importance);
    
    # Collective learning
    sys->print("\n[Distributed] Collective learning...\n");
    
    patterns_local := "pattern_A" :: "pattern_B" :: nil;
    learned_local := cogkernel->learn(local_sys, patterns_local);
    sys->print("  Node 1 learned: %d patterns\n", learned_local);
    
    patterns_remote := "pattern_C" :: "pattern_D" :: nil;
    learned_remote := cogkernel->learn(remote_sys, patterns_remote);
    sys->print("  Node 2 learned: %d patterns\n", learned_remote);
    
    # Synchronized thinking
    sys->print("\n[Distributed] Synchronized thinking...\n");
    
    thought_local := cogkernel->think(local_sys, "distributed cognition");
    sys->print("  Node 1 thought: %s\n", thought_local);
    
    thought_remote := cogkernel->think(remote_sys, "distributed cognition");
    sys->print("  Node 2 thought: %s\n", thought_remote);
    
    # Collective reflection
    sys->print("\n[Distributed] Collective reflection...\n");
    
    insight_local := cogkernel->sys_reflect(local_sys);
    if (insight_local != nil)
        sys->print("  Node 1 insight: %s\n", insight_local.content);
    
    insight_remote := cogkernel->sys_reflect(remote_sys);
    if (insight_remote != nil)
        sys->print("  Node 2 insight: %s\n", insight_remote.content);
    
    # Network statistics
    sys->print("\n[Network] Distributed cognitive statistics:\n");
    sys->print("  Total nodes: 2\n");
    sys->print("  Total atoms: %d\n", 
               len local_sys.atomspace.atoms + len remote_sys.atomspace.atoms);
    sys->print("  Total links: %d\n",
               len local_sys.atomspace.links + len remote_sys.atomspace.links);
    sys->print("  Combined attention: %.1f STI\n",
               local_sys.config.attention_budget + remote_sys.config.attention_budget);
    
    # 9P Protocol demonstration
    sys->print("\n[9P] Distributed filesystem layout:\n");
    sys->print("  /cogkernel/local/atomspace/    - Local knowledge\n");
    sys->print("  /cogkernel/local/reasoning/    - Local inferences\n");
    sys->print("  /cogkernel/local/attention/    - Local attention\n");
    sys->print("  /mnt/node2/atomspace/          - Remote knowledge (via 9P)\n");
    sys->print("  /mnt/node2/reasoning/          - Remote inferences (via 9P)\n");
    sys->print("  /mnt/node2/attention/          - Remote attention (via 9P)\n");
    
    sys->print("\n[9P] Network transparency:\n");
    sys->print("  Reading /mnt/node2/atomspace/nodes/concept_X triggers\n");
    sys->print("  remote kernel operation and returns atom over network.\n");
    sys->print("  Writing to /mnt/node2/reasoning/deductions triggers\n");
    sys->print("  remote reasoning and returns inferences.\n");
    
    sys->print("\n=== Collective Intelligence Achieved ===\n");
    sys->print("Multiple cognitive kernels thinking together as one mind.\n");
    sys->print("Distributed cognition through 9P protocol enables:\n");
    sys->print("  - Shared knowledge spaces\n");
    sys->print("  - Distributed reasoning\n");
    sys->print("  - Collective attention allocation\n");
    sys->print("  - Synchronized learning\n");
    sys->print("  - Network-transparent thinking\n\n");
}
