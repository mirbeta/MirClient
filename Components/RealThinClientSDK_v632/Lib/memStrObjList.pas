{
  "Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memStrObjList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, rtcTypes, memPtrPool;

type
  itemType=RtcString;
  infoType=TObject;

{$I sort\types.inc}

type
  tStrObjList={$I sort\class.inc};

implementation

const
  itemMin:itemType='';
  infoNil:infoType=NIL;

function tStrObjList.{$I sort\Empty.inc};

function tStrObjList.{$I sort\NewNode.inc};

procedure tStrObjList.{$I sort\PoolSize.inc};

procedure tStrObjList.{$I sort\DelNode.inc};

constructor tStrObjList.{$I sort\Create.inc};

procedure tStrObjList.{$I sort\Change.inc};

procedure tStrObjList.{$I sort\RemoveThis.inc};

procedure tStrObjList.{$I sort\RemoveAll.inc};

destructor tStrObjList.{$I sort\Destroy.inc};

function tStrObjList.{$I sort\Search.inc};

function tStrObjList.{$I sort\SearchMin.inc};

function tStrObjList.{$I sort\SearchMax.inc};

function tStrObjList.{$I sort\SearchL.inc};

function tStrObjList.{$I sort\SearchG.inc};

function tStrObjList.{$I sort\SearchLE.inc};

function tStrObjList.{$I sort\SearchGE.inc};

procedure tStrObjList.{$I sort\InsertSplit.inc};

procedure tStrObjList.{$I sort\Insert.inc};

procedure tStrObjList.{$I sort\RemoveAddP.inc};

function tStrObjList.{$I sort\RemoveGetP.inc};

procedure tStrObjList.{$I sort\Remove.inc};

function tStrObjList.{$I sort\Count.inc};

end.
