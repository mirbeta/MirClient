{
  "Balanced Binary search List"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memStringObjList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, rtcTypes, memPtrPool;

type
  itemType=RtcWideString;
  infoType=TObject;

{$I sort\types.inc}

type
  tStringObjList={$I sort\class.inc};

implementation

const
  itemMin:itemType='';
  infoNil:infoType=NIL;

function tStringObjList.{$I sort\Empty.inc};

function tStringObjList.{$I sort\NewNode.inc};

procedure tStringObjList.{$I sort\PoolSize.inc};

procedure tStringObjList.{$I sort\DelNode.inc};

constructor tStringObjList.{$I sort\Create.inc};

procedure tStringObjList.{$I sort\Change.inc};

procedure tStringObjList.{$I sort\RemoveThis.inc};

procedure tStringObjList.{$I sort\RemoveAll.inc};

destructor tStringObjList.{$I sort\Destroy.inc};

function tStringObjList.{$I sort\Search.inc};

function tStringObjList.{$I sort\SearchMin.inc};

function tStringObjList.{$I sort\SearchMax.inc};

function tStringObjList.{$I sort\SearchL.inc};

function tStringObjList.{$I sort\SearchG.inc};

function tStringObjList.{$I sort\SearchLE.inc};

function tStringObjList.{$I sort\SearchGE.inc};

procedure tStringObjList.{$I sort\InsertSplit.inc};

procedure tStringObjList.{$I sort\Insert.inc};

procedure tStringObjList.{$I sort\RemoveAddP.inc};

function tStringObjList.{$I sort\RemoveGetP.inc};

procedure tStringObjList.{$I sort\Remove.inc};

function tStringObjList.{$I sort\Count.inc};

end.
