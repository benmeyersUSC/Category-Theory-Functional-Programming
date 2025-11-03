#include <vector>
#include <string>
#include <iostream>
#include <functional>
#include <utility>
#include <map>

struct Node;
struct Edge;

struct Edge {
	Node* to;
	int len;
	Edge(Node* f, Node* t, int l): to(t), len(l) {}
};

struct Node {
	std::string name;
	std::vector<Edge> children;
	Node(std::string n):name(n) {}
	void addNode(Node& n, int l) {
		children.push_back(Edge(this, &n, l));
	}
};


int minElem(std::vector<int> v) {
	if (v.size() == 0) {
		return -1;
	}
	int m = v[0];
	for (int i = 1; i < v.size(); i++) {
		m = std::min(v[i], m);
	}
	return m;
}

template<typename In1, typename Out1>
std::function<Out1(In1, In1)> memoize(std::function<Out1(In1, In1)> f) {
	// memoized version makes this and saves it
	// the lambda it returns captures
	std::map<std::pair<In1,In1>, Out1>* memo = new std::map<std::pair<In1,In1>, Out1>();

	return [f, memo](In1 x0, In1 x1) mutable { // mutable means captures can be changed
		auto key = std::make_pair(x0, x1);
		if (memo->count(key) == 0) {
			// to see if it's working!
			std::cout << "[adding SOMETHING to the memo]\n";
			memo->operator[](key) = f(x0,x1);
		}
		return memo->operator[](key);
	};
};




int minDist(Node* a, Node* f) {
	std::cout << "calling minDist\n";
	if (a->name != f->name) {
		std::vector<int> childDists;
		for (Edge& e : a->children) {
			childDists.push_back(e.len + minDist(e.to, f));
		}
		return minElem(childDists);
	}
	else {
		return 0;
	}
}

int main() {

	std::function<int(Node*,Node*)> MinDist;
	MinDist = memoize<Node*, int>(
	[&MinDist](Node* a, Node* f) {
		std::cout << "calling MinDist\n";
		if (a->name != f->name) {
			std::vector<int> childDists;
			for (Edge& e : a->children) {
				childDists.push_back(e.len + MinDist(e.to, f));
			}
			return minElem(childDists);
		}
		else {
			return 0;
		}
	}
	          );


	Node A = Node("A");
	Node B = Node("B");
	Node C = Node("C");
	Node D = Node("D");
	Node E = Node("E");
	Node F = Node("F");

	A.addNode(B, 3);
	A.addNode(C, 2);
	B.addNode(D, 4);
	B.addNode(E, 5);
	B.addNode(C, 1);
	C.addNode(D, 6);
	C.addNode(E, 3);

	E.addNode(F, 4);
	E.addNode(D, 2);
	D.addNode(F, 5);

	std::cout << MinDist(&A, &F) << std::endl;
	std::cout << "-----------------------------------\n";
	std::cout << minDist(&A, &F) << std::endl;

	return 0;
}