{
  "Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memStringIntList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, rtcTypes, memPtrPool;

type
  itemType=RtcWideString;
  infoType=longint;

{$I sort\types.inc}

type
  tStringIntList={$I sort\class.inc};

implementation

const
  itemMin:itemType='';
  infoNil:infoType=-1;

function tStringIntList.{$I sort\Empty.inc};

function tStringIntList.{$I sort\NewNode.inc};

procedure tStringIntList.{$I sort\PoolSize.inc};

procedure tStringIntList.{$I sort\DelNode.inc};

constructor tStringIntList.{$I sort\Create.inc};

procedure tStringIntList.{$I sort\Change.inc};

procedure tStringIntList.{$I sort\RemoveThis.inc};

procedure tStringIntList.{$I sort\RemoveAll.inc};

destructor tStringIntList.{$I sort\Destroy.inc};

function tStringIntList.{$I sort\Search.inc};

function tStringIntList.{$I sort\SearchMin.inc};

function tStringIntList.{$I sort\SearchMax.inc};

function tStringIntList.{$I sort\SearchL.inc};

function tStringIntList.{$I sort\SearchG.inc};

function tStringIntList.{$I sort\SearchLE.inc};

function tStringIntList.{$I sort\SearchGE.inc};

procedure tStringIntList.{$I sort\InsertSplit.inc};

procedure tStringIntList.{$I sort\Insert.inc};

procedure tStringIntList.{$I sort\RemoveAddP.inc};

function tStringIntList.{$I sort\RemoveGetP.inc};

procedure tStringIntList.{$I sort\Remove.inc};

function tStringIntList.{$I sort\Count.inc};

end.
