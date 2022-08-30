{
  "Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memStrIntList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, rtcTypes, memPtrPool;

type
  itemType=RtcString;
  infoType=longint;

{$I sort\types.inc}

type
  tStrIntList={$I sort\class.inc};

implementation

const
  itemMin:itemType='';
  infoNil:infoType=-1;

function tStrIntList.{$I sort\Empty.inc};

function tStrIntList.{$I sort\NewNode.inc};

procedure tStrIntList.{$I sort\PoolSize.inc};

procedure tStrIntList.{$I sort\DelNode.inc};

constructor tStrIntList.{$I sort\Create.inc};

procedure tStrIntList.{$I sort\Change.inc};

procedure tStrIntList.{$I sort\RemoveThis.inc};

procedure tStrIntList.{$I sort\RemoveAll.inc};

destructor tStrIntList.{$I sort\Destroy.inc};

function tStrIntList.{$I sort\Search.inc};

function tStrIntList.{$I sort\SearchMin.inc};

function tStrIntList.{$I sort\SearchMax.inc};

function tStrIntList.{$I sort\SearchL.inc};

function tStrIntList.{$I sort\SearchG.inc};

function tStrIntList.{$I sort\SearchLE.inc};

function tStrIntList.{$I sort\SearchGE.inc};

procedure tStrIntList.{$I sort\InsertSplit.inc};

procedure tStrIntList.{$I sort\Insert.inc};

procedure tStrIntList.{$I sort\RemoveAddP.inc};

function tStrIntList.{$I sort\RemoveGetP.inc};

procedure tStrIntList.{$I sort\Remove.inc};

function tStrIntList.{$I sort\Count.inc};

end.
