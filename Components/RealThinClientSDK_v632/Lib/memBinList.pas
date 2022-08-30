{
  "Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memBinList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  memPtrPool;

type
  itemType=RtcIntPtr;
  infoType=RtcIntPtr;

{$I sort\types.inc}

type
  tBinList={$I sort\class.inc};

implementation

const
  itemMin:itemType=0;
  infoNil:infoType=0;

function tBinList.{$I sort\Empty.inc};

function tBinList.{$I sort\NewNode.inc};

procedure tBinList.{$I sort\PoolSize.inc};

procedure tBinList.{$I sort\DelNode.inc};

constructor tBinList.{$I sort\Create.inc};

procedure tBinList.{$I sort\Change.inc};

procedure tBinList.{$I sort\RemoveThis.inc};

procedure tBinList.{$I sort\RemoveAll.inc};

destructor tBinList.{$I sort\Destroy.inc};

function tBinList.{$I sort\Search.inc};

function tBinList.{$I sort\SearchMin.inc};

function tBinList.{$I sort\SearchMax.inc};

function tBinList.{$I sort\SearchL.inc};

function tBinList.{$I sort\SearchG.inc};

function tBinList.{$I sort\SearchLE.inc};

function tBinList.{$I sort\SearchGE.inc};

procedure tBinList.{$I sort\InsertSplit.inc};

procedure tBinList.{$I sort\Insert.inc};

procedure tBinList.{$I sort\RemoveAddP.inc};

function tBinList.{$I sort\RemoveGetP.inc};

procedure tBinList.{$I sort\Remove.inc};

function tBinList.{$I sort\Count.inc};

end.
