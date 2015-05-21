{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- NOTE: expects AWS credentials to be set via the envirnoment variables
-- AWS_ACCESS_KEY and AWS_SECRET_KEY, or as per
-- http://hackage.haskell.org/package/aws-0.12/docs/Aws.html#v:baseConfiguration

-- Based on https://github.com/aristidb/aws/blob/master/Examples/DynamoDb.hs

-- ---------------------------------------------------------------------
import           Aws
import           Aws.DynamoDb.Commands
import           Aws.DynamoDb.Core
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Data.Conduit
import qualified Data.Conduit.List     as C
import qualified Data.Text             as T
import           Network.HTTP.Conduit  (withManager)
-- ---------------------------------------------------------------------

{- Working through http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GettingStartedDynamoDB.html


Use case 1: Product Catalog

Suppose you want to store product information in DynamoDB. Each product you
store has its own set of properties, and accordingly, you need to store
different information about each of these products. DynamoDB is a NoSQL
database: Except for a required common primary key, individual items in a table
can have any number of attributes. This enables you to save all the product data
in the same table. So you will create a ProductCatalog table that uses Id as the
primary key and stores information for products such as books and bicycles in
the table. Id is a numeric attribute and hash type primary key. After creating
the table, in the next step you will write code to retrieve items from this
table. Note that while you can retrieve an item, you cannot query the table. To
query the table, the primary key must be of the hash and range type.

Table Name                    : ProductCatalog
Primary Key                   : (Id, ... )
Type                          : Hash
Hash Attribute Name           : Id
Hash Attribute Type           : Number
Range Attribute Name and Type : N/A
Provisioned Throughput:
  Read capacity units: 10
  Write capacity units: 5

-}
createProductCatalogTableAndWait :: IO ()
createProductCatalogTableAndWait = do
  let req0 = createTable "ProductCatalog"
        [AttributeDefinition "Id" AttrNumber]
        (HashOnly "Id")
        (ProvisionedThroughput 10 5)
  resp0 <- runCommand req0
  print resp0

  print "Waiting for table to be created"
  threadDelay (30 * 1000000)

  let req1 = DescribeTable "ProductCatalog"
  resp1 <- runCommand req1
  print resp1

-- ---------------------------------------------------------------------

productCatalogTable :: CreateTable
productCatalogTable = createTable "ProductCatalog"
        [AttributeDefinition "Id" AttrNumber]
        (HashOnly "Id")
        (ProvisionedThroughput 10 5)

forumTable :: CreateTable
forumTable = createTable "Forum"
        [AttributeDefinition "Name" AttrString]
        (HashOnly "Name")
        (ProvisionedThroughput 10 5)

threadTable :: CreateTable
threadTable = createTable "Thread"
        [AttributeDefinition "ForumName" AttrString
        ,AttributeDefinition "Subject" AttrString]
        (HashAndRange "ForumName" "Subject")
        (ProvisionedThroughput 10 5)

replyTable :: CreateTable
replyTable = createTable "Reply"
        [AttributeDefinition "Id" AttrString
        ,AttributeDefinition "ReplyDateTime" AttrString
        ,AttributeDefinition "PostedBy" AttrString
        ]
        (HashAndRange "Id" "ReplyDateTime")
        (ProvisionedThroughput 10 5)

replyLocalIndex = LocalSecondaryIndex
       "PostedBy"
       (HashAndRange "Id" "PostedBy")
       ProjectKeysOnly

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- Aws.baseConfiguration

  wrap $ createTableAndWait productCatalogTable
  wrap $ createTableAndWait forumTable
  wrap $ createTableAndWait threadTable
  wrap $ createTableAndWait replyTable {createLocalSecondaryIndexes = [replyLocalIndex]}

-- ---------------------------------------------------------------------

runCommand r = do
    cfg <- Aws.baseConfiguration
    let
       myServiceConfig :: DdbConfiguration NormalQuery
       myServiceConfig = ddbHttps euWest1
    -- Aws.simpleAws cfg debugServiceConfig r
    Aws.simpleAws cfg myServiceConfig r
    -- Aws.simpleAws cfg _ r

euWest1 = Region "dynamodb.eu-west-1.amazonaws.com" "eu-west-1"

-- ---------------------------------------------------------------------

wrap :: IO () -> IO ()
wrap cmd = cmd `catch` (\e@DdbError{} -> putStrLn $ "Error:" ++ show e)

createTableAndWait :: CreateTable -> IO ()
createTableAndWait req0 = do
  resp0 <- runCommand req0
  print resp0

  print "Waiting for table to be created"
  threadDelay (30 * 1000000)

  let req1 = DescribeTable (createTableName req0) 
  resp1 <- runCommand req1
  print resp1
