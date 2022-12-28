package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*
import scala.util.control.NonFatal
import scala.util.control.NonLocalReturns.*

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


import cps.*
import cps.monads.{*,given}

import cps.testconfig.given
import cps.util.FutureCompleter


class TestReturningExamples {

    type UserId = String

    case class UserInfo(blacklisted: Boolean, limit: Long)

    case class Item(name: String)

    case class ItemInfo(available:Boolean, cost: Long, name: String)
    
    case class Bucket(messages: IndexedSeq[String], items:IndexedSeq[ItemInfo], cost: Long) {
      def withMessage(message:String): Bucket =
        copy(messages = messages appended message)

      def withItem(itemInfo: ItemInfo): Bucket =
        copy(items = items appended itemInfo, cost = cost + itemInfo.cost)

      def isEmpty: Boolean =
        items.isEmpty

    }

    object Bucket {
      val empty = Bucket(IndexedSeq.empty,IndexedSeq.empty,0L)
    }

    case class Config(minItemCost: Long)

    trait DataAPI  {

      def config: Config

      def fetchUserInfo(userId: UserId): Future[UserInfo]

      def fetchItemInfo(item: Item): Future[ItemInfo]

    } 
  

    class MockDataAPI(funUsers: UserId => Future[UserInfo], funItems: String => Future[ItemInfo], val config:Config) extends DataAPI {


      def fetchUserInfo(userId: UserId): Future[UserInfo] =
        funUsers(userId)
       
      def fetchItemInfo(item: Item): Future[ItemInfo] =
        funItems(item.name)

    }


 
    //  
    def fillbucketWithinBudget(userId: UserId, items: Seq[Item], budget: Long, api: DataAPI): Future[Either[String,Bucket]] = async[Future]{
      returning {
        val userInfo = await(api.fetchUserInfo(userId))
        val config = api.config

        if (userInfo.blacklisted)
          throwReturn(Left("User is blacklisted"):Either[String,Bucket])

        val bucket = items.foldLeft(Bucket.empty){ (bucket, item) =>
          val itemInfo = await(api.fetchItemInfo(item))
          if (!itemInfo.available) then
            bucket.withMessage(s"item ${item.name} is not available")
          else if (itemInfo.cost + bucket.cost > budget) then
            bucket.withMessage(s"item ${item.name} is out of budget")
          else
            val newBucket = bucket.withItem(itemInfo)
            if (newBucket.cost + config.minItemCost > budget) then
              throwReturn(Right(newBucket):Either[String,Bucket])
            newBucket
        }  

        if (bucket.isEmpty) then
          throwReturn(Left(s"No items available: ${bucket.messages.mkString}"):Either[String,Bucket])

        Right(bucket)  
      }
    }

    
    @Test 
    def testThrowReturnFromFold() = {
      val mockDataApi = MockDataAPI(
        (id: UserId) => Future successful UserInfo(false,100),
        name => Future successful ItemInfo(true,50,name),
        Config(minItemCost = 50)
      )

      val f = async[Future] {
        val items = (1 to 100).map(x => Item(s"item$x"))
        val r = await(fillbucketWithinBudget("1", items, 100, mockDataApi))
        assert(r.isRight)
        r match
          case Right(bucket) =>
            assert(bucket.items.size==2)
          case Left(bucket) =>
            assert(false,"assumed that answer is right")
      }
      FutureCompleter(f)

    }

    import scala.collection.mutable.ArrayBuffer

    class MutableBucket{
       
      val messages: ArrayBuffer[String] = ArrayBuffer.empty
      val items: ArrayBuffer[ItemInfo] = ArrayBuffer.empty

      def addMessage(message:String): Unit =
        messages.append(message)

      def addItem(itemInfo: ItemInfo): Unit =
        items.append(itemInfo)

      def isEmpty: Boolean =
        items.isEmpty

    }
    object MutableBucket{

      def empty = new MutableBucket()

    }

    def fillMutableBucketWithinBudget(userId: UserId, items: Seq[Item], budget: Long, api: DataAPI): Future[Either[String,MutableBucket]] = async[Future] {
        returning {
          val userInfo = await(api.fetchUserInfo(userId))
          val config = api.config
          
          if (userInfo.blacklisted)
            throwReturn(Left("User is blacklisted"):Either[String,MutableBucket])

          var remainingBudget = budget
          val bucket = MutableBucket.empty
          for(item <- items) {
            val itemInfo = await(api.fetchItemInfo(item))
            if (!itemInfo.available) then
              bucket.addMessage(s"item ${item.name} is not available")
            else if (itemInfo.cost > remainingBudget) then
              bucket.addMessage(s"item ${item.name} is out of budget")
            else 
              bucket.addItem(itemInfo)
              remainingBudget = remainingBudget - itemInfo.cost
              if (remainingBudget < config.minItemCost) then
                throwReturn(Right(bucket):Either[String,MutableBucket])
          }  

          if (bucket.isEmpty) then
            throwReturn(Left("No items available"):Either[String,MutableBucket])

          Right(bucket)
        }
    }



}