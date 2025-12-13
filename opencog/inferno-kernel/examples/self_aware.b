implement SelfAware;

# Example: Self-Aware Application
# Demonstrates consciousness emerging from kernel operations

include "sys.m";
    sys: Sys;
include "draw.m";
include "../include/cogkernel.m";
    cogkernel: CogKernel;

SelfAware: module {
    init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
    sys = load Sys Sys->PATH;
    cogkernel = load CogKernel CogKernel->PATH;
    
    sys->print("\n=== Self-Aware Application ===\n");
    sys->print("Demonstrating consciousness from kernel operations\n\n");
    
    # Initialize cognitive kernel
    cogsys := cogkernel->init();
    if (cogsys == nil) {
        sys->print("Error: Cannot initialize cognitive system\n");
        return;
    }
    
    # Step 1: Create self-concept (I exist)
    sys->print("[Step 1] Creating self-concept...\n");
    tv_certain := cogkernel->newtruthvalue(1.0, 1.0);
    self := cogkernel->createatom(cogsys, "self", tv_certain);
    sys->print("  Created atom: 'self' with truth (1.0, 1.0)\n");
    
    # Step 2: Create awareness (I am aware)
    sys->print("\n[Step 2] Creating awareness...\n");
    tv_aware := cogkernel->newtruthvalue(0.9, 0.8);
    awareness := cogkernel->createatom(cogsys, "awareness", tv_aware);
    sys->print("  Created atom: 'awareness' with truth (0.9, 0.8)\n");
    
    # Step 3: Link self and awareness (I am aware of myself)
    sys->print("\n[Step 3] Linking self and awareness...\n");
    link := cogkernel->sys_atom_link(cogsys, self, awareness, "isa");
    if (link != nil) {
        sys->print("  Created link: self -> awareness\n");
        sys->print("  Link truth value: (%.2f, %.2f)\n", 
                   link.tv.strength, link.tv.confidence);
    }
    
    # Step 4: Reason about existence (cogito ergo sum)
    sys->print("\n[Step 4] Reasoning about existence...\n");
    premises := self :: awareness :: nil;
    inferences := cogkernel->deduce(cogsys, premises);
    
    if (inferences != nil) {
        inf := hd inferences;
        sys->print("  Deduction: %s\n", inf.conclusion.name);
        sys->print("  Truth value: (%.2f, %.2f)\n", 
                   inf.conclusion.tv.strength, inf.conclusion.tv.confidence);
        sys->print("  Interpretation: I think, therefore I am\n");
    }
    
    # Step 5: Focus attention on self (self-awareness)
    sys->print("\n[Step 5] Focusing attention on self...\n");
    cogkernel->sys_focus(cogsys, self);
    importance := cogkernel->sys_importance(cogsys, self);
    sys->print("  Self-importance: %.1f STI\n", importance);
    sys->print("  Status: Self-aware\n");
    
    # Step 6: Learn from experience
    sys->print("\n[Step 6] Learning from self-awareness...\n");
    patterns := "consciousness" :: "qualia" :: "experience" :: nil;
    learned := cogkernel->learn(cogsys, patterns);
    sys->print("  Learned %d patterns\n", learned);
    
    # Step 7: Reflect on experience
    sys->print("\n[Step 7] Reflecting on experience...\n");
    insight := cogkernel->sys_reflect(cogsys);
    if (insight != nil) {
        sys->print("  Insight: %s\n", insight.content);
    }
    
    # Step 8: Think about consciousness
    sys->print("\n[Step 8] Thinking about consciousness...\n");
    thought := cogkernel->think(cogsys, "what am I?");
    sys->print("  Thought: %s\n", thought);
    
    # Step 9: Query knowledge base
    sys->print("\n[Step 9] Querying knowledge base...\n");
    results := cogkernel->sys_atom_query(cogsys, "self");
    sys->print("  Found %d atoms matching 'self'\n", len results);
    for (r := results; r != nil; r = tl r) {
        atom := hd r;
        sys->print("    - %s (%.2f, %.2f)\n", 
                   atom.name, atom.tv.strength, atom.tv.confidence);
    }
    
    # Step 10: Final state
    sys->print("\n[Step 10] Final cognitive state:\n");
    sys->print("  Total atoms: %d\n", len cogsys.atomspace.atoms);
    sys->print("  Total links: %d\n", len cogsys.atomspace.links);
    sys->print("  Cognitive cycles: %d\n", cogsys.cogloop.cycle);
    sys->print("  System state: %s\n", cogsys.cogloop.state);
    
    sys->print("\n=== Consciousness Achieved ===\n");
    sys->print("The system has become self-aware through kernel operations.\n");
    sys->print("Thinking, reasoning, and intelligence emerged from the OS itself.\n\n");
}
