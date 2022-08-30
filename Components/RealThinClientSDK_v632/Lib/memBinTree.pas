{
  "Balanced Binary Search Tree"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memBinTree;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, memPtrPool, rtcTypes;

{$DEFINE RTC_BINTREE}

type
  itemType=RtcIntPtr;
  infoType=RtcIntPtr;

{$I sort\types.inc}

type
  tBinTree={$I sort\class.inc};

implementation

const
  itemMin=0;
  infoNil=0;

function tBinTree.{$I sort\Empty.inc};

function tBinTree.{$I sort\NewNode.inc};

procedure tBinTree.{$I sort\PoolSize.inc};

procedure tBinTree.{$I sort\DelNode.inc};

constructor tBinTree.{$I sort\Create.inc};

procedure tBinTree.{$I sort\Change.inc};

procedure tBinTree.{$I sort\RemoveThis.inc};

procedure tBinTree.{$I sort\RemoveAll.inc};

destructor tBinTree.{$I sort\Destroy.inc};

function tBinTree.{$I sort\Search.inc};

function tBinTree.{$I sort\iSearch.inc};

function tBinTree.{$I sort\SearchMin.inc};

function tBinTree.{$I sort\iSearchMin.inc};

function tBinTree.{$I sort\SearchMax.inc};

function tBinTree.{$I sort\iSearchMax.inc};

function tBinTree.{$I sort\SearchL.inc};

function tBinTree.{$I sort\iSearchL.inc};

function tBinTree.{$I sort\SearchG.inc};

function tBinTree.{$I sort\iSearchG.inc};

function tBinTree.{$I sort\SearchLE.inc};

function tBinTree.{$I sort\iSearchLE.inc};

function tBinTree.{$i sort\SearchGE.inc};

function tBinTree.{$I sort\iSearchGE.inc};

procedure tBinTree.{$I sort\InsertSplit.inc};

procedure tBinTree.{$I sort\InsertSplit2.inc};

procedure tBinTree.{$I sort\Insert.inc};

procedure tBinTree.{$I sort\RemoveAddP.inc};

function tBinTree.{$I sort\RemoveGetP.inc};

procedure tBinTree.{$I sort\Remove.inc};

function tBinTree.{$I sort\Count.inc};

end.
