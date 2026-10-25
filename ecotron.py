import matplotlib.pyplot as plt
import networkx as nx

# Create a graph to represent Ecotrons and trees
G = nx.Graph()

num_ecotrons = 12
trees_per_ecotron = 6
pear_varieties = ["Conference", "Doyenne du Comice"]

# Add nodes for Ecotrons and their trees
for ecotron in range(1, num_ecotrons + 1):
    ecotron_name = f"Ecotron {ecotron}"
    G.add_node(ecotron_name, color="blue", size=800)  # Ecotron nodes

    for tree in range(1, trees_per_ecotron + 1):
        variety = pear_varieties[0] if tree <= 3 else pear_varieties[1]  # First 3 trees are Conference, last 3 are Doyenne
        tree_name = f"{variety} Tree {tree} (E{ecotron})"
        G.add_node(tree_name, color="green" if variety == "Conference" else "orange", size=500)
        G.add_edge(ecotron_name, tree_name)

# Get node colors and sizes
colors = [G.nodes[node]["color"] for node in G.nodes]
sizes = [G.nodes[node]["size"] for node in G.nodes]

# Draw the graph without labels
plt.figure(figsize=(10, 7))
pos = nx.spring_layout(G, seed=42)  # Position nodes
nx.draw(G, pos, node_color=colors, node_size=sizes, edge_color="gray")

# Custom legend at bottom-right corner
legend_labels = {"Ecotron": "blue", "Conference Tree": "green", "Doyenne du Comice Tree": "orange"}
legend_handles = [plt.Line2D([0], [0], marker='o', markersize=10, color='w', markerfacecolor=color, label=label)
                  for label, color in legend_labels.items()]

plt.legend(handles=legend_handles, title="Legend", loc="lower right", frameon=True)
plt.title("Study Design: Ecotrons & Pear Trees")
plt.show()