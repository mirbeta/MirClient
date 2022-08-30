{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express tree view printed dataset                        }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSGRID AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxtrprds;

{$I cxVer.inc}

interface

uses
  Types, Classes, SysUtils, dxmdaset, Windows, DB, Variants;

type

TdxDBTreePrintData = class;

TdxDBTreePrintDataLink = class(TDataLink)
private
  FPrintData : TdxDBTreePrintData;
protected
  procedure ActiveChanged; override;
end;

TdxDBTreePrintData = class(TdxMemData)
private
  FDataLink : TdxDBTreePrintDataLink;
  FKeyFieldName : String;
  FParentFieldName : String;
  FLevelCount : Integer;
  FRootValue : Variant;
  FRootStrValue : String;
  FMaxLevelCount : Integer;

  function GetTreeDataSource : TDataSource;
  procedure SetTreeDataSource(Value : TDataSource);
  procedure SetKeyFieldName(Value : String);
  procedure SetLevelCount(Value : Integer);
  procedure SetParentFieldName(Value : String);
  procedure SetRootStrValue(Value : String);

  procedure RefreshStructure;
protected
  procedure CopyStructure(ASource: TDataSet);
  procedure DoAfterOpen; override;

  property DataLink : TdxDBTreePrintDataLink read FDataLink;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure Loaded; override;

  property MaxLevelCount : Integer read FMaxLevelCount;
published
  property DataSource read GetTreeDataSource write SetTreeDataSource;
  property KeyField : String read FKeyFieldName write SetKeyFieldName;
  property LevelCount : Integer read FLevelCount write SetLevelCount;
  property ParentField : String read FParentFieldName write SetParentFieldName;
  property RootValue : String read FRootStrValue write SetRootStrValue;
end;

procedure FillDBTreePrintedDataSet(ds1 : TdxDBTreePrintData; ds2 : TDataSet);

const
  cDBTreeLevelFieldName = 'dx$level';
  cDBTreeRecNoFieldName = 'dx$recno';
  cDBTreeHasChildrenFieldName = 'dx$haschildren';

implementation

procedure TdxDBTreePrintDataLink.ActiveChanged;
begin
  if FPrintData <> nil then
    FPrintData.RefreshStructure;
end;

constructor TdxDBTreePrintData.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TdxDBTreePrintDataLink.Create;
  FDataLink.FPrintData := self;
  FLevelCount := 0;
  FRootValue := NULL;
  FRootStrValue := '';
  FMaxLevelCount := 0;

end;

destructor TdxDBTreePrintData.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TdxDBTreePrintData.Loaded;
begin
  inherited Loaded;
  RefreshStructure;
end;

function TdxDBTreePrintData.GetTreeDataSource : TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TdxDBTreePrintData.DoAfterOpen;
begin
  inherited DoAfterOpen;
  FMaxLevelCount := 0;
  FillDBTreePrintedDataSet(self, FDataLink.DataSet);
end;

procedure TdxDBTreePrintData.SetTreeDataSource(Value : TDataSource);
begin
  if(Value <> FDataLink.DataSource) then begin
    FDataLink.DataSource := Value;
    RefreshStructure;
  end;
end;

procedure TdxDBTreePrintData.SetKeyFieldName(Value : String);
begin
  if(FKeyFieldName <> Value) then begin
    FKeyFieldName := Value;
    RefreshStructure;
  end;
end;

procedure TdxDBTreePrintData.SetLevelCount(Value : Integer);
begin
  if(Value < 0) then Value := 0;
  if(FLevelCount <> Value) then begin
    FLevelCount := Value;
    if Not(csLoading in ComponentState) And Active then begin
      Close;
      Open;
    end;
  end;
end;

procedure TdxDBTreePrintData.SetParentFieldName(Value : String);
begin
  if(FParentFieldName <> Value) then begin
    FParentFieldName := Value;
    RefreshStructure;
  end;
end;

procedure TdxDBTreePrintData.SetRootStrValue(Value : String);
begin
  if(FRootStrValue <> Value) then begin
    FRootStrValue := Value;
    if(FRootStrValue = '') then
      FRootValue := NULL
    else FRootValue := FRootStrValue;
    if Not(csLoading in ComponentState) And Active then begin
      Close;
      Open;
    end;
  end;
end;

procedure TdxDBTreePrintData.CopyStructure(ASource: TDataSet);
var
  i: Integer;
begin
  CheckInactive;
  for i := FieldCount - 1 downto 0 do
    if Fields[i] <> RecIdField then
      Fields[i].Free;

  if (ASource = nil) then Exit;

  FieldDefs := ASource.FieldDefs;

  for i := 0 to FieldDefs.Count - 1 do begin
    if SupportedFieldType(FieldDefs.Items[i].DataType) then
      FieldDefs.Items[i].CreateField(Self);
  end;

  for i := 0 to FieldCount - 1 do
    Fields[i].Required := False;

  FieldDefs.Add(cDBTreeLevelFieldName, ftInteger, 0, False);
  FieldDefs.Items[FieldDefs.Count - 1].CreateField(self);
  FieldDefs.Add(cDBTreeRecNoFieldName, ftInteger, 0, False);
  FieldDefs.Items[FieldDefs.Count - 1].CreateField(self);
  FieldDefs.Add(cDBTreeHasChildrenFieldName, ftBoolean, 0, False);
  FieldDefs.Items[FieldDefs.Count - 1].CreateField(self);
end;

procedure TdxDBTreePrintData.RefreshStructure;
begin
  if (csLoading in ComponentState) then exit;
  if Active then
    Close;
  if(FDataLink.DataSet <> Nil) and FDataLink.DataSet.Active
  And (FDataLink.DataSet.FindField(FKeyFieldName) <> Nil)
  And (FDataLink.DataSet.FindField(FParentFieldName) <> Nil) then
    CopyStructure(FDataLink.DataSet);
end;

type
TDBTreePrintedNodes = class;

TDBTreePrintedNode = class
private
  Destroying : Boolean;
  Owner : TDBTreePrintedNodes;
  KeyValue : Variant;
  ParentValue : Variant;
  Level : Integer;
  Parent : TDBTreePrintedNode;
  RecNumber : Integer;
  ChildList : TList;

  procedure SetChild(Value : TDBTreePrintedNode);
  procedure SetLevel(Value : Integer);
public
  constructor Create(AOwner : TDBTreePrintedNodes; AKeyValue, AParentValue : Variant);
  destructor Destroy; override;
  function HasAsParent(prNode : TDBTreePrintedNode) : Boolean;
end;

TDBTreePrintedNodes = class
private
  PrintedDS : TdxDBTreePrintData;
  SourceDS : TDataSet;
  List : TList;
  RootList : TList;
  SortedList : TList;
protected
  function IndexOf(Value : Variant) : Integer;
  procedure DestroyItems;
  function FindNearest(Value : Variant; Var Index : Integer) : Boolean;
  function GetMaxKeyValue : Variant;
  procedure SortNodes(List : TList);
  procedure UpdateNodes;
public
  constructor Create(APrintedDS : TdxDBTreePrintData; ASourceDS : TDataSet);
  destructor Destroy; override;

  procedure CreateStructure;
end;

// Internal methods
function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

function VarFirstMore(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 >= V2;
  except
  end;
end;

function VarFirstMoreEx(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 > V2;
  except
  end;
end;

{TDBTreePrintedNode}
constructor TDBTreePrintedNode.Create(AOwner : TDBTreePrintedNodes; AKeyValue, AParentValue : Variant);
Var
  Index : Integer;
begin
  Owner := AOwner;
  KeyValue := AKeyValue;
  ParentValue := AParentValue;
  ChildList := Nil;
  RecNumber := Owner.List.Count;
  Owner.FindNearest(KeyValue, Index);
  if(Index < 0) then Index := 0;
  Owner.List.Insert(Index, self);
  Index := Owner.IndexOf(ParentValue);
  Level := 0;
  if(Index > -1) And (TDBTreePrintedNode(Owner.List[Index]) <> self) then
    TDBTreePrintedNode(Owner.List[Index]).SetChild(self)
  else begin
    Parent := Nil;
    Owner.RootList.Add(self);
  end;
  Owner.SortedList.Add(self);
end;

destructor TDBTreePrintedNode.Destroy;
var
  i : Integer;
begin
  Destroying := True;
  if(ChildList <> Nil) then
  begin
    for i := 0 to ChildList.Count - 1 do
      TDBTreePrintedNode(ChildList[i]).Parent := Nil;
    ChildList.Free;
    ChildList := nil;
  end;
  if(Parent <> nil) and not Parent.Destroying and (Parent.ChildList  <> nil) then
  begin
    Parent.ChildList.Remove(self);
    if(Parent.ChildList.Count = 0) then
    begin
      Parent.ChildList.Free;
      Parent.ChildList := Nil;
    end;
  end;

  inherited Destroy;
end;

procedure TDBTreePrintedNode.SetChild(Value : TDBTreePrintedNode);
begin
  if(Value = self) then exit;
  if(ChildList = Nil) then
    ChildList := TList.Create;
  ChildList.Add(Value);
  Value.Parent := self;
  Owner.RootList.Remove(Value);
  Value.SetLevel(Level + 1);
end;

procedure TDBTreePrintedNode.SetLevel(Value : Integer);
Var
  i : Integer;
begin
  Level := Value;
  if(ChildList <> Nil) then
    for i := 0 to ChildList.Count - 1 do
      TDBTreePrintedNode(ChildList[i]).SetLevel(Level + 1);
end;

function TDBTreePrintedNode.HasAsParent(prNode : TDBTreePrintedNode) : Boolean;
Var
  Node : TDBTreePrintedNode;
begin
  Result := False;
  Node := Parent;
  while Node <> Nil do begin
    if(Node = prNode) then begin
      Result := True;
      break;
    end;
    Node := Node.Parent;
  end;
end;

{TDBTreePrintedNodes}
constructor TDBTreePrintedNodes.Create(APrintedDS : TdxDBTreePrintData; ASourceDS : TDataSet);
begin
  PrintedDS := APrintedDS;
  SourceDS := ASourceDS;
  List := TList.Create;
  RootList := TList.Create;
  SortedList := TList.Create;
end;

destructor TDBTreePrintedNodes.Destroy;
begin
  DestroyItems;
  List.Free;
  RootList.Free;
  SortedList.Free;
  inherited;
end;

procedure TDBTreePrintedNodes.DestroyItems;
Var
  item : TDBTreePrintedNode;
begin
  while List.Count > 0 do begin
    item := TDBTreePrintedNode(List.Last);
    List.Delete(List.Count - 1);
    item.Free;
  end;
  RootList.Clear;
end;

procedure TDBTreePrintedNodes.CreateStructure;
Var
  bm : TBookMark;
  i, FRecNumber, NewRecNo : Integer;
  KeyField, ParentField : TField;
  FieldType : TFieldType;
  AFieldD, AFieldS : TField;
  ListD, ListS : TList;
begin
  DestroyItems;
  if(PrintedDS <> nil) and (SourceDS <> nil) then
  begin
    KeyField := SourceDS.FindField(PrintedDS.FKeyFieldName);
    ParentField := SourceDS.FindField(PrintedDS.FParentFieldName);
    if (KeyField = nil) Or (ParentField = nil) then Exit;

    FieldType := KeyField.DataType;
    with PrintedDS do
    begin
      if not VarIsNull(FRootValue) then
        case FieldType of
          ftSmallint: VarCast(FRootValue, FRootValue,  varSmallint);
          ftInteger, ftWord, ftAutoInc: VarCast(FRootValue, FRootValue,  varInteger);
          ftFloat, ftCurrency: VarCast(FRootValue, FRootValue,  varDouble);
        else
          VarCast(FRootValue, FRootValue,  varString);
        end;
      DisableControls;
    end;

    with SourceDS do
    begin
      DisableControls;
      bm := GetBookMark;
      First;
      while not EOF do begin
        TDBTreePrintedNode.Create(Self, KeyField.Value, ParentField.Value);
        Next;
      end;
      UpdateNodes;
      First;
      FRecNumber := 1;
      for i := 0 to List.Count - 1 do begin
        PrintedDS.Append;
        PrintedDS.Post;
      end;
      ListD := TList.Create;
      ListS := TList.Create;
      for i := 0 to PrintedDS.FieldCount - 4 do
      begin
        AFieldD := FindField(PrintedDS.Fields[i].FieldName);
        AFieldS := PrintedDS.FindField(PrintedDS.Fields[i].FieldName);
        if (AFieldD <> nil) and (AFieldS <> nil) then
        begin
          ListD.Add(AFieldD);
          ListS.Add(AFieldS);
        end;
      end;

      while Not EOF do
      begin
        if (SortedList[FRecNumber - 1] <> nil) then begin
          NewRecNo := TDBTreePrintedNode(SortedList[FRecNumber - 1]).RecNumber;
          PrintedDS.RecNo := NewRecNo;
          PrintedDS.Edit;
          for i := 0 to ListD.Count - 1 do
            TField(ListS[i]).Assign(TField(ListD[i]));
          PrintedDS.FindField(cDBTreeLevelFieldName).AsInteger := TDBTreePrintedNode(SortedList[FRecNumber - 1]).Level;
          PrintedDS.FindField(cDBTreeRecNoFieldName).AsInteger := TDBTreePrintedNode(SortedList[FRecNumber - 1]).RecNumber;
          if(PrintedDS.FMaxLevelCount < TDBTreePrintedNode(SortedList[FRecNumber - 1]).Level) then
            PrintedDS.FMaxLevelCount := TDBTreePrintedNode(SortedList[FRecNumber - 1]).Level;
          PrintedDS.FindField(cDBTreeHasChildrenFieldName).AsBoolean := TDBTreePrintedNode(SortedList[FRecNumber - 1]).ChildList <> Nil;
          PrintedDS.Post;
        end;
        Next;
        Inc(FRecNumber);
      end;
      GotoBookMark(bm);
      FreeBookMark(bm);
      EnableControls;
    end;
    ListD.Free;
    ListS.Free;
    PrintedDS.First;
    PrintedDS.EnableControls;
  end;
end;

function TDBTreePrintedNodes.IndexOf(Value : Variant) : Integer;
Var
  i : Integer;
begin
  if(FindNearest(Value, i)) then
    Result := i
  else Result := -1;
end;

function TDBTreePrintedNodes.FindNearest(Value : Variant; Var Index : Integer) : Boolean;
var
  Min, Max : LongInt;
begin
  Result := False;
  if (List.Count = 0) Or VarIsNull(Value)
  Or VarFirstMoreEx( TDBTreePrintedNode(List[0]).KeyValue, Value) then begin
    Index := -1;
    exit;
  end;

  if VarFirstMoreEx(Value, GetMaxKeyValue) then begin
    Index := List.Count;
    Exit;
  end;

  Min := 0;
  Max := List.Count - 1;

  repeat
    if ((Max - Min) = 1) then begin
      if(Min = Index) then Min := Max;
      if(Max = Index) then Max := Min;
    end;
    Index := Min + ((Max - Min) div 2);
    if VarEquals(Value, TDBTreePrintedNode(List[Index]).KeyValue) then break;
    if VarFirstMore(Value, TDBTreePrintedNode(List[Index]).KeyValue) then
      Min := Index
    else  Max := Index;
  until (Min = Max);
  if Not VarEquals(Value, TDBTreePrintedNode(List[Index]).KeyValue) then begin
    if (Index < List.Count - 1) And VarFirstMore(Value, TDBTreePrintedNode(List[Index]).KeyValue) then
       Inc(Index);
  end else Result := True;
end;

function TDBTreePrintedNodes.GetMaxKeyValue : Variant;
begin
  Result := NULL;
  if(List.Count > 0) then begin
    Result := TDBTreePrintedNode(List[List.Count - 1]).KeyValue;
  end;
end;

procedure TDBTreePrintedNodes.UpdateNodes;
Var
  i, Index, NextValue, OldCount : Integer;
  RootNode, prNode : TDBTreePrintedNode;

  procedure MakeSortNodes(Node : TDBTreePrintedNode);
  Var
    ii : Integer;
  begin
    if(Node.ChildList <> Nil) then begin
      SortNodes(Node.ChildList);
      for ii := 0 to Node.ChildList.Count - 1 do
        MakeSortNodes(TDBTreePrintedNode(Node.ChildList[ii]));
    end;
  end;

  procedure  SetRecNumber(Node : TDBTreePrintedNode);
  Var
    ii : Integer;
  begin
    if(Node.ChildList <> Nil) then begin
      for ii := 0 to Node.ChildList.Count - 1 do begin
        TDBTreePrintedNode(Node.ChildList[ii]).RecNumber := NextValue;
        Inc(NextValue);
        SetRecNumber(TDBTreePrintedNode(Node.ChildList[ii]));
      end;
    end;
  end;

  procedure RemoveFromList(Node : TDBTreePrintedNode);
  var
    flag : Integer;
  begin
     if(Node.ChildList <> nil) then
     begin
      flag := 0;
       while (Node.ChildList <> nil) and (Node.ChildList.Count > flag) do
         if(RootNode <> TDBTreePrintedNode(Node.ChildList[flag])) then
           RemoveFromList(TDBTreePrintedNode(Node.ChildList[flag]))
         else Inc(flag);
     end;
    List.Remove(Node);
    SortedList[Node.RecNumber] := Nil;
    Node.Free;
  end;

  procedure SetParentToNil(AList: TList);
  var
    I: Integer;
  begin
    for I := 0 to AList.Count - 1 do
      TDBTreePrintedNode(AList[I]).Parent := nil;
  end;

begin
  i := 0;
  while i < RootList.Count do begin
    Index := IndexOf(TDBTreePrintedNode(RootList[i]).ParentValue);
    OldCount := RootList.Count;
    if(Index > -1)then
      TDBTreePrintedNode(List[Index]).SetChild(TDBTreePrintedNode(RootList[i]));
    if (i < RootList.Count) And (OldCount = RootList.Count)
    And (TDBTreePrintedNode(RootList[i]).Parent = Nil) then
      Inc(i);
  end;

  if Not VarIsNull(PrintedDS.FRootValue)
  And FindNearest(PrintedDS.FRootValue, Index) then begin
    RootNode := TDBTreePrintedNode(List[Index]);
    RootList.Clear;
    i := 0;
    while (i < List.Count) do begin
      prNode := TDBTreePrintedNode(List[i]);
      if (RootNode <> prNode) And Not (prNode.HasAsParent(RootNode)) then
        RemoveFromList(prNode)
      else Inc(i);
    end;
    RootList.Add(RootNode);
    RootNode.SetLevel(0);
  end;

  if (PrintedDS.FLevelCount > 0) then begin
    i := 0;
    Index := PrintedDS.FLevelCount - 1;
    while (i < List.Count) do begin
      if(TDBTreePrintedNode(List[i]).Level >= Index) then begin
        prNode := TDBTreePrintedNode(List[i]);
        if (prNode.ChildList <> Nil)then begin
          SetParentToNil(prNode.ChildList);
          prNode.ChildList.Free;
          prNode.ChildList := Nil;
        end;
        if(prNode.Level = Index) then
          Inc(i)
        else begin
          List.Delete(i);
          SortedList[prNode.RecNumber] := Nil;
          prNode.Free;
        end;
      end else Inc(i);
    end;
  end;

  SortNodes(RootList);
  for i := 0 to RootList.Count - 1 do
    MakeSortNodes(TDBTreePrintedNode(RootList[i]));

  NextValue := 1;
  for i := 0 to RootList.Count - 1 do begin
    TDBTreePrintedNode(RootList[i]).RecNumber := NextValue;
    Inc(NextValue);
    SetRecNumber(TDBTreePrintedNode(RootList[i]));
  end;

end;

procedure TDBTreePrintedNodes.SortNodes(List : TList);

  function GetValue(i : Integer) : Integer;
  begin
    Result := TDBTreePrintedNode(List[i]).RecNumber;
  end;

  procedure Sort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := GetValue((Lo + Hi) div 2);
    repeat
      while GetValue(Lo) < Mid do Inc(Lo);
      while GetValue(Hi) > Mid do Dec(Hi);
      if Lo <= Hi then begin
        List.Exchange(Lo, Hi);
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then Sort(iLo, Hi);
    if Lo < iHi then Sort(Lo, iHi);
  end;

begin
  if List.Count > 0 then
    Sort(0, List.Count - 1);
end;

procedure FillDBTreePrintedDataSet(ds1 : TdxDBTreePrintData; ds2 : TDataSet);
Var
 st : TDBTreePrintedNodes;
begin
 st := TDBTreePrintedNodes.Create(ds1, ds2);
 st.CreateStructure;
 st.Free;
end;

end.

