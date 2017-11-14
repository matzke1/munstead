module munstead.core.itree;

import core.exception: RangeError;
import std.algorithm: filter, map, until;
import std.container.rbtree: RedBlackTree;
import std.functional: unaryFun;
import std.range: array, chain, drop, retro, take, zip;
import std.range.primitives: ElementType, isInputRange;
import std.typecons: tuple;

// Base class for containers that are indexed by intervals.
//   T = storage node including the interval where the node is stored.
//   intervalExtractor = unary function to obtain an interval from a storage node
class IntervalTree(T, alias intervalExtractor)
if (is(typeof(unaryFun!intervalExtractor(T.init))))
{
  alias Interval = typeof(unaryFun!intervalExtractor(T.init));

private:
  alias Node = T;
  alias interval_ = unaryFun!intervalExtractor;
  enum less_ = function bool(const Node a, const Node b) { return interval_(a).startsBefore(interval_(b)); };
  alias Tree = RedBlackTree!(Node, less_);

  Tree tree_;
  size_t nElmts_;
  
public:
  // Create empty tree
  this() {
    tree_ = new Tree;
  }

  // Is tree empty?
  pure bool empty() const @property @safe {
    return tree_.length == 0; // cannot call tree_.empty with a const object
  }

  // Number of elements (not intervals) in tree
  pure size_t length() const @property @safe {
    if (nElmts_ == 0 && !empty)
      throw new RangeError;
    return nElmts_;
  }

  // Number of maximally-sized intervals
  pure size_t nIntervals() const @property @safe {
    return tree_.length;
  }

  // Nodes of the tree
  pure nodes() const @property @safe {
    return tree_[];
  }

  // Remove all elements from the set
  pure void clear() @safe {
    tree_.clear();
    nElmts_ = 0;
  }

  // Minimum and maximum elements contained in the set
  pure Interval hull() const @safe {
    if (empty)
      return Interval();

    Interval.Value lo = tree_[].take(1).map!(node => interval_(node).least).array[0];
    Interval.Value hi = tree_[].retro.take(1).map!(node => interval_(node).greatest).array[0];
    return Interval.hull(lo, hi);
  }

  // True if all specified values exist in this set, or if the specified interval is empty
  pure bool existsAll(Node needle) const @safe {
    Interval interval = interval_(needle);
    if (interval.empty)
      return true;
    auto overlaps = overlapRange(needle);
    if (overlaps.empty)
      return false;
    foreach (node; overlaps) {
      if (interval_(node).least > interval.least)
        return false;
      interval = interval.greaterThan(interval_(node).greatest);
    }
    return interval.empty;
  }

  // True if any of the specified values exist. False if the interval is empty.
  pure bool existsAny(Node needle) const @safe {
    return !overlapRange(needle).empty;
  }

protected:
  // Range of nodes that overlap with some interval. FIXME: It seems like it should be possible to do this with one
  // lookup instead of three.
  pure overlapRange(Node needle) @safe {
    Interval needleInterval = interval_(needle);
    return chain(tree_.lowerBound(needle).retro.until!(a => interval_(a).disjoint(needleInterval)), // 0 or 1 element
                 tree_.equalRange(needle), // 0 or 1 element
                 tree_.upperBound(needle).until!(a => interval_(a).disjoint(needleInterval)) // 0 or more elements
                 );
  }
  pure overlapRange(Node needle) const @safe { // FIXME: use inout instead of copy-paste
    Interval needleInterval = interval_(needle);
    return chain(tree_.lowerBound(needle).retro.until!(a => interval_(a).disjoint(needleInterval)), // 0 or 1 element
                 tree_.equalRange(needle), // 0 or 1 element
                 tree_.upperBound(needle).until!(a => interval_(a).disjoint(needleInterval)) // 0 or more elements
                 );
  }

  // Adjust the count of the number of elements in this set. It is assumed that the supplied range of intervals are
  // disjoint and each is non-empty. They don't need to be sorted.
  pure void adjustCount(string op, Range)(Range range) @safe
  if (op == "+" || op == "-") {
    foreach (node; range) {
      assert(!interval_(node).empty);
      if (interval_(node) == Interval.whole() && Interval.Value.sizeof == size_t.sizeof) {
        nElmts_ = 0;
      } else {
        static if (op == "+") {
          nElmts_ += interval_(node).length;
        } else {
          nElmts_ -= interval_(node).length;
        }
      }
    }
  }

  // Removes the specified nodes from the tree. The intervals contained in the specified nodes must exactly exist in the
  // tree. The nodes do not need to be sorted, although sorted nodes might be faster at some point in the future if
  // std.container.rbtree.RedBlackTree supports that.
  pure void removeTreeNodes(Range)(Range nodes) @safe {
    adjustCount!"-"(nodes);
    tree_.removeKey(nodes);
  }

  // Inserts the specified nodes into the tree. The intervals contained in the nodes must not overlap with any nodes
  // already in the tree.  The nodes don't need to be sorted, although at some point in the future
  // std.container.rbtree.RedBlackTree might be more effient if the were.
  pure void insertTreeNodes(Range)(Range nodes) @safe {
    adjustCount!"+"(nodes);
    tree_.insert(nodes);
  }
}
