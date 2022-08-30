{
  "Pointer Pool"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit memPtrPool;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTypes;

type
  tPoolItemType=pointer;

  tPtrPoolElems = array of tPoolItemType;

  tPtrPool = class(TObject)
    private
      pObjs:tPtrPoolElems;
      fCount,fSize:integer;
      procedure SetSize(x:integer);
    public
      constructor Create(Size:integer=0);
      destructor Destroy; override;
      function Put(const x:tPoolItemType):boolean; // if Pool is full, return FALSE and Free object memory
      function Get:tPoolItemType; // if Pool is empty, return FALSE (you have to create the Object)
      property Size:integer read fSize write SetSize;
      property Count:integer read fCount;
    end;

implementation

{ tPrtPool }

constructor tPtrPool.Create(Size: integer);
  begin
  inherited Create;
  fSize:=Size;
  if fSize>0 then
    SetLength(pObjs,fSize);
  fCount:=0;
  end;

destructor tPtrPool.Destroy;
  begin
  fCount:=0;
  if fSize>0 then
    begin
    SetLength(pObjs,0);
    fSize:=0;
    end;
  inherited;
  end;

function tPtrPool.Get:tPoolItemType;
  begin
  if fCount>0 then
    begin
    Dec(fCount);
    Result:=pObjs[fCount];
    end
  else
    Result:=nil;
  end;

function tPtrPool.Put(const x: tPoolItemType): boolean;
  begin
  if fCount<fSize then
    begin
    pObjs[fCount]:=x;
    Inc(fCount);
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure tPtrPool.SetSize(x: integer);
  begin
  if x<>fSize then
    begin
    fSize:=x;
    SetLength(pObjs,fSize);
    end;
  end;

end.
