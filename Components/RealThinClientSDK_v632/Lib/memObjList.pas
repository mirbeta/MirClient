{
  "Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memObjList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, memPtrPool, rtcTypes;

type
  itemType = RtcIntPtr;
  infoType = TObject;

{$I sort\types.inc}

type
  tObjList={$I sort\class.inc};

implementation

const
  itemMin:itemType=0;
  infoNil:infoType=nil;

function tObjList.{$I sort\Empty.inc};

function tObjList.{$I sort\NewNode.inc};

procedure tObjList.{$I sort\PoolSize.inc};

procedure tObjList.{$I sort\DelNode.inc};

constructor tObjList.{$I sort\Create.inc};

procedure tObjList.{$I sort\Change.inc};

procedure tObjList.{$I sort\RemoveThis.inc};

procedure tObjList.{$I sort\RemoveAll.inc};

destructor tObjList.{$I sort\Destroy.inc};

function tObjList.{$I sort\Search.inc};

function tObjList.{$I sort\SearchMin.inc};

function tObjList.{$I sort\SearchMax.inc};

function tObjList.{$I sort\SearchL.inc};

function tObjList.{$I sort\SearchG.inc};

function tObjList.{$I sort\SearchLE.inc};

function tObjList.{$I sort\SearchGE.inc};

procedure tObjList.{$I sort\InsertSplit.inc};

procedure tObjList.{$I sort\Insert.inc};

procedure tObjList.{$I sort\RemoveAddP.inc};

function tObjList.{$I sort\RemoveGetP.inc};

procedure tObjList.{$I sort\Remove.inc};

function tObjList.{$I sort\Count.inc};

end.
