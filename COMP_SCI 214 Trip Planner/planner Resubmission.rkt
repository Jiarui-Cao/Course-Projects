#lang dssl2

# Final project: Trip Planner


let eight_principles = ["Know your rights.", 
"Acknowledge your sources.",
"Protect your work.",
"Avoid suspicion.",
"Do your own work.",
"Never falsify a record or permit another person to do so.",
"Never fabricate data, citations, or experimental results.",
"Always tell the truth when discussing your work with your instructor."]

test 'HonorCode':
    assert eight_principles.len() == 8



import cons
import sbox_hash
import 'project-lib/binheap.rkt'
import 'project-lib/stack-queue.rkt'
import 'project-lib/graph.rkt'
import 'project-lib/dictionaries.rkt'




#######################
##### Basic Types #####
#######################

#  - Latitudes and longitudes are numbers:
let Lat?  = num?
let Lon?  = num?

#  - Point-of-interest categories and names are strings:
let Cat?  = str?
let Name? = str?

### Raw Entity Types ###

#  - Raw positions are 2-element vectors with a latitude and a longitude
let RawPos? = TupC[Lat?, Lon?]

#  - Raw road segments are 4-element vectors with the latitude and
#    longitude of their first endpoint, then the latitude and longitude
#    of their second endpoint
let RawSeg? = TupC[Lat?, Lon?, Lat?, Lon?]

#  - Raw points-of-interest are 4-element vectors with a latitude, a
#    longitude, a point-of-interest category, and a name
let RawPOI? = TupC[Lat?, Lon?, Cat?, Name?]

### Contract Helpers ###

# ListC[T] is a list of `T`s (linear time):
let ListC = Cons.ListC
# List of unspecified element type (constant time):
let List? = Cons.list?


interface TRIP_PLANNER:

    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(
            self,
            dst_cat:  Cat?           # point-of-interest category
        )   ->        ListC[RawPos?] # positions of the POIs

    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.
    def plan_route(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_name: Name?          # name of goal
        )   ->        ListC[RawPos?] # path to goal

    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_cat:  Cat?,          # point-of-interest category
            n:        nat?           # maximum number of results
        )   ->        ListC[RawPOI?] # list of nearby POIs

        
        

   
             
######################
# Additional Structs #
######################
            
struct posn:
    let lat: Lat?
    let lon: Lon?
    
struct point:                    
    let cat: Cat?
    let name: Name?
    
struct data:
    let vertex
    let pred
    let dist 


#list of points# <--------> #position# <--------> #vertex#    (bidirectional mapping between 3 items)
    
    
    


#############################
##### TripPlanner CLASS #####
#############################
    
class TripPlanner (TRIP_PLANNER):
    #let all_POI
    let MyMap          # WUGraph for positions (their respective vertices) and road segments
    let posn_to_v      # Dictionary with key = position (posn?), value = vertex (nat?)
    let v_to_posn      # Dictionary with key = vertex (nat?), value = position (posn?)   
    let posn_to_node   # Dictionary with key = position (posn?), value = linked list of points (ListC[point?])
    let node_to_posn   # Dictionary with key = linked list of points (ListC[point?]), value = position (posn?)
    let posn_count
    
    def __init__(self, VecSeg, VecPOI):
              
        let l = 2 * len(VecSeg)
        self.posn_to_node = HashTable(l, make_sbox_hash())
        self.node_to_posn = HashTable(l, make_sbox_hash())
        self.posn_to_v = HashTable(l, make_sbox_hash())
        self.v_to_posn = HashTable(l, make_sbox_hash())
        self.posn_count = 0
    
        # Build the 4 dictionaries, by iterating through the elements of vec[road segments]
        for road in VecSeg:
            let p1 = posn(road[0], road[1]) # position 1
            let p2 = posn(road[2], road[3]) # position 2
            let n1 = None # node 1
            let n2 = None # node 2
        
            # Iterate through the elements of vec[points-of-interest] to construct the 
            # node corresponding to each position, each node being a linked list of pt(cat, name)
            for poi in VecPOI:
                if poi[0] == p1.lat and poi[1] == p1.lon:
                    let node = point(poi[2], poi[3])
                    n1 = cons(node, n1)
                elif poi[0] == p2.lat and poi[1] == p2.lon:
                    let node = point(poi[2], poi[3])
                    n2 = cons(node, n2)
            
            # Construct the two dictionaries doubly linking posn(lat, lon) and the linked list of 
            # all points that belong to this position; meanwhile, construct the other two dictionaries
            # doubly linking posn(lat, lon) and the vertex(nat?) that represents it in MyMap
            if not self.posn_to_node.mem?(p1):
                self.posn_to_node.put(p1, n1)
                self.posn_to_v.put(p1, self.posn_count)
                self.v_to_posn.put(self.posn_count, p1)
                self.posn_count = self.posn_count + 1                              
            
            if not self.posn_to_node.mem?(p2):
                self.posn_to_node.put(p2, n2)
                self.posn_to_v.put(p2, self.posn_count)
                self.v_to_posn.put(self.posn_count, p2)
                self.posn_count = self.posn_count + 1    
                
        for poi in VecPOI:
            self.node_to_posn.put(poi[3], posn(poi[0], poi[1]))
              
                
        # Build the main graph that represents the connection between all positions posn(lat, lon)
        self.MyMap = WuGraph(self.posn_count)
        for road in VecSeg:
            let v1 = self.posn_to_v.get(posn(road[0], road[1]))
            let v2 = self.posn_to_v.get(posn(road[2], road[3]))
            let w = ((road[0] - road[2])**2 + (road[1] - road[3])**2)**0.5  
            self.MyMap.set_edge(v1, v2, w)
        
      
                             
            
    ### Given a POI category; returns the position of all POI in the given category    
    def locate_all(self, category):
        #print("There are %d positions in total", self.posn_count)
        let lst = None
        for i in range(self.posn_count):
            let position = self.v_to_posn.get(i) 
            let node = self.posn_to_node.get(position)
            if curr_query(node, category, λ x, y: x.data.cat == y):
                lst = cons([position.lat, position.lon], lst)
        return lst
   
             
        
                
    ### Given a starting position and name of a POI; returns shortest path from start to the given POI
    def plan_route(self, latitude, longitude, name): 
        if not self.node_to_posn.mem?(name): return None     
        let start = self.posn_to_v.get(posn(latitude, longitude))
        let end = self.posn_to_v.get(self.node_to_posn.get(name))
        let SSSP = dijkstra(self.MyMap, start)
        let output = None
        if SSSP[end][2] == None and start != end:
            return None
        # With edge cases checked, now let's start looping!    
        let curr = end
        while not curr == None:
            let p = self.v_to_posn.get(curr)
            output = cons([p.lat, p.lon], output)
            curr = SSSP[curr][2]
        return output
        
        
        
              
    ### Given a starting position, POI category, and limit n;
    ### returns the (up to) n POI in the given category nearest to the starting position
    def find_nearby(self, latitude, longitude, cat, limit):
        let start = self.posn_to_v.get(posn(latitude, longitude))
        let SSSP = dijkstra(self.MyMap, start)
        heap_sort(SSSP, λ x, y: x[1] < y[1])
        
        let l = limit
        let output = None
        for i in range(len(SSSP)): 
            let position = self.v_to_posn.get(SSSP[i][0])
            if SSSP[i][2] != None or SSSP[i][0] == start:
                let curr = self.posn_to_node.get(position)
                while l != 0 and curr != None:
                    let category = curr.data.cat
                    let name = curr.data.name
                    if category == cat:
                        output = cons([position.lat, position.lon, category, name], output)
                        l = l - 1
                    curr = curr.next        
        return output
                
            
        
        
        
    
    
    
    
    
    
    
############################
##### Helper Functions #####
############################

def curr_query(list, item, f?):
    let curr = list
    while not curr == None:
        if f?(curr, item):
            return True
        curr = curr.next
    return False        


                
# Dijkstra's algorithm second try: returns a matrix
def dijkstra(graph, start):
    let output = [None; graph.len()]
    # Output is a matrix with |V| rows and 3 columns: vertex, dist, pred
    for i in range(len(output)):
        output[i] = [i, inf, None] 
    output[start][1] = 0
    let todo = BinHeap(Cons.len(graph.get_all_edges()), λ x, y: x[1] < y[1])
    let done = [False; graph.len()]
        
    todo.insert(output[start])
    while not todo.len() == 0:
        let curr = todo.find_min()
        let curr_v = curr[0]
        todo.remove_min()
        if done[curr_v] == False:
            done[curr_v] == True
            let E = graph.get_adjacent(curr_v)
            while E != None:
                let u = E.data
                let weight = graph.get_edge(curr_v, u)
                if weight != None and weight != inf:
                    if curr[1] + weight < output[u][1]:
                        output[u][1] = curr[1] + weight
                        output[u][2] = curr_v
                        todo.insert(output[u])
                E = E.next    
    return output            
    
    
    
        
        
    
    
    
    
    
    
    
    
#########################
##### All the Tests #####
#########################  
    
    
def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pelmeni"]])

test 'My first locate_all test':
    assert my_first_example().locate_all("food") == cons([0,1], None)
    assert my_first_example().locate_all("bar") == cons([0,0], None)
    assert my_first_example().locate_all("Target") == None
    

test 'My first plan_route test':
   assert my_first_example().plan_route(0, 0, "Pelmeni") == cons([0,0], cons([0,1], None))
   assert my_first_example().plan_route(1, 0, "Pelmeni") == cons([1,0], cons([0,0], cons([0,1], None)))
   

test 'My first find_nearby test':
    assert my_first_example().find_nearby(0, 0, "food", 1) == cons([0,1, "food", "Pelmeni"], None)
    assert my_first_example().find_nearby(1, 0, "bar", 2) == cons([0,0, "bar", "The Empty Bottle"], None)


    
def example_from_handout():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0], [0,1, 0,2], [1,0, 1,1], [0,1, 1,1], [1,1, 1,2], [0,2, 1,2], [1,2, 1,3], [1,3, -0.2,3.3]],
                       [[0,0, "food", "Sandwiches"],
                        [0,1, "food", "Pasta"],
                        [1,1, "bank", "Local Credit Union"],
                        [1,3, "bar", "Bar None"],
                        [1,3, "bar", "H Bar"],
                        [-0.2, 3.3, "food", "Burritos"]])
                        
test "locate_all test 2":
    let a = example_from_handout()
    assert a.locate_all("food") == cons([-0.2,3.3], cons([0,1], cons([0,0], None)))
    assert a.locate_all("bar") == cons([1,3], None)
    assert a.locate_all("bank") == cons([1,1], None)
    assert a.locate_all("barger") == None
    
    
test "plan_route test 2":
    let a = example_from_handout()
    assert a.plan_route(0, 0, "Sandwiches") == cons([0,0], None)
    assert a.plan_route(0, 1, "Sandwiches") == cons([0,1], cons([0,0], None))
    assert a.plan_route(1, 1, "Sandwiches") == cons([1,1], cons([0,1], cons([0,0], None)))
    assert a.plan_route(1, 1, "Burritos") == cons([1,1], cons([1,2], cons([1,3], cons([-0.2,3.3], None))))
    assert a.plan_route(1, 1, "Sushi") == None
    assert a.plan_route(-0.2, 3.3, "Pasta") == cons([-0.2,3.3], cons([1,3], cons([1,2], cons([1,1], cons([0,1], None)))))
    
    
test "find_nearby test 2":
    let a = example_from_handout()
    assert a.find_nearby(1, 3, "food", 1) == cons([-0.2, 3.3, "food", "Burritos"], None)
    assert a.find_nearby(0, 0, "food", 1) == cons([0,0, "food", "Sandwiches"], None)
    assert a.find_nearby(0, 2, "food", 1) == cons([0,1, "food", "Pasta"], None)
    assert a.find_nearby(0, 2, "food", 2) == cons([0,0, "food", "Sandwiches"], cons([0,1, "food", "Pasta"], None))
    assert a.find_nearby(0, 2, "food", 3) == cons([-0.2, 3.3, "food", "Burritos"], cons([0,0, "food", "Sandwiches"], cons([0,1, "food", "Pasta"], None)))
    assert a.find_nearby(0, 2, "food", 4) == a.find_nearby(0, 2, "food", 3)
    assert a.find_nearby(0, 2, "bar", 1) == cons([1,3, "bar", "H Bar"], None)
    assert a.find_nearby(0, 2, "bar", 2) == cons([1,3, "bar", "Bar None"], cons([1,3, "bar", "H Bar"], None))
    assert a.find_nearby(0, 2, "bar", 3) == cons([1,3, "bar", "Bar None"], cons([1,3, "bar", "H Bar"], None))
    assert a.find_nearby(0, 2, "school", 1) == None
    
    





test "Destination not reachable (route)":    
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert Cons.to_vec(result) == []

test "MST is not SSSP (route)":
    let tp = TripPlanner(
      [[-1.1, -1.1, 0, 0],
       [0, 0, 3, 0],
       [3, 0, 3, 3],
       [3, 3, 3, 4],
       [0, 0, 3, 4]],
      [[0, 0, 'food', 'Sandwiches'],
       [3, 0, 'bank', 'Union'],
       [3, 3, 'barber', 'Judy'],
       [3, 4, 'barber', 'Tony']])
    let result = tp.plan_route(-1.1, -1.1, 'Tony')
    assert Cons.to_vec(result) == [[-1.1, -1.1], [0, 0], [3, 4]]

test "Relevant POI not reachable (nearby)":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [4, 0, 'food', 'Jollibee'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.find_nearby(0, 0, 'food', 1)
    assert Cons.to_vec(result) == []
      
test "MSP is not SSSP (nearby)":
    let tp = TripPlanner(
      [[-1.1, -1.1, 0, 0],
       [0, 0, 3, 0],
       [3, 0, 3, 3],
       [3, 3, 3, 4],
       [0, 0, 3, 4]],
      [[0, 0, 'food', 'Sandwiches'],
       [3, 0, 'bank', 'Union'],
       [3, 3, 'barber', 'Judy'],
       [3, 4, 'barber', 'Tony']])
    let result = tp.find_nearby(-1.1, -1.1, 'barber', 1)
    assert Cons.to_vec(result) == [[3, 4, 'barber', 'Tony']]
    
test "2 relevant POIs, 1 reachable (nearby)":
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [4, 0, 'food', 'Jollibee'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.find_nearby(0, 0, 'barber', 2)
    assert Cons.to_vec(result) == [[3, 0, 'barber', 'Tony']]



    
