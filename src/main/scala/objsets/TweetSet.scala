package objsets

//import common._
//import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {
  val any: Tweet

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p, new Empty)
  }

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   */
  def descendingByRetweet: TweetList

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def isEmpty: Boolean
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def mostRetweeted = throw new NoSuchElementException("List Is Empty!")

  def descendingByRetweet: TweetList = Nil

  def union(that: TweetSet): TweetSet = that

  def isEmpty = true

  val any: Tweet = new Tweet("","",-1)
}


class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    var newAcc: TweetSet = acc
    if(p(elem)) newAcc = newAcc.incl(elem)
    newAcc = left.filterAcc(p, newAcc)
    newAcc = right.filterAcc(p, newAcc)
    newAcc
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  /*
  todo The more often a tweet is “re-tweeted” (that is, repeated by a different user with or without
   additions), the more influential it is.

The goal of this part of the exercise is to add a method descendingByRetweet to TweetSet which
 should produce a linear sequence of tweets (as an instance of class TweetList), ordered by their
 number of retweets:

def descendingByRetweet: TweetList

  This method reflects a common pattern when transforming data structures. While traversing one data
  structure (in this case, a TweetSet), we’re building a second data structure (here, an instance of
  class TweetList). The idea is to start with the empty list Nil (containing no tweets), and to find
   the tweet with the most retweets in the input TweetSet. This tweet is removed from the TweetSet
  (that is, we obtain a new TweetSet that has all the tweets of the original set except for the
   tweet that was “removed”; this immutable set operation, remove, is already implemented for you),
   and added to the result list by creating a new Cons. After that, the process repeats itself, but now
   we are searching through a TweetSet with one less tweet.

Hint: start by implementing the method mostRetweeted which returns the most popular tweet of a TweetSet.
   */
  def mostRetweeted: Tweet = {

    def mostRetweetedHelper(current: Tweet, acc: TweetSet): Tweet = {
      if(acc.isEmpty) return current
      val max = if (current.retweets < acc.any.retweets) elem else current
      mostRetweetedHelper(max, acc.remove(acc.any))
    }

    mostRetweetedHelper(elem,this)
  }

  //  def descendingByRetweet: TweetList = {
  //     var res: TweetList = Nil
  //     var list: TweetSet = this
  //
  //    while (list isInstanceOf NonEmpty){
  //
  //       val next = mostRetweeted
  //       res = new Cons(mostRetweeted, res)
  //       list = remove(mostRetweeted)
  //     }
  //     return res
  //  }

  def union(that: TweetSet): TweetSet = {
    var res = that
    if(!that.contains(elem)){
      res = that.incl(elem)
    }
    val l = left.union(res)
    val r = right.union(l)
    r
  }

  def isEmpty = false

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   */
  override def descendingByRetweet: TweetList = ???

  val any: Tweet = elem
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  /*
  todo 4 Tying everything together

In the last step of this assignment your task is to detect influential tweets in a set of recent
tweets. We are providing you with a TweetSet containing several hundred tweets from popular tech
 news sites in the past few days, located in the TweetReader object (file “TweetReader.scala”).
 TweetReader.allTweets returns an instance of TweetSet containing a set of all available tweets.

Furthermore, you are given two lists of keywords. The first list corresponds to keywords associated
 with Google and Android smartphones, while the second list corresponds to keywords associated
  with Apple and iOS devices. Your objective is to detect which platform has generated more
  interest or activity in the past few days.

As a first step, use the functionality you implemented in the first parts of this assignment
 to create two different TweetSets, googleTweets and appleTweets. The first TweetSet,
  googleTweets, should contain all tweets that mention (in their “text”) one of the
  keywords in the google list. The second TweetSet, appleTweets, should contain all
  tweets that mention one of the keyword in the apple list. Their signature is as follows:

lazy val googleTweets: TweetSet
lazy val appleTweets: TweetSet

Hint: use the exists method of List and contains method of class java.lang.String.

From the union of those two TweetSets, produce trending, an instance of class TweetList
 representing a sequence of tweets ordered by their number of retweets:

lazy val trending: TweetList
   */
  lazy val googleTweets: TweetSet = ???
  lazy val appleTweets: TweetSet = ???

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = ???
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
