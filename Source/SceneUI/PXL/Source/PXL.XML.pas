unit PXL.XML;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

{$INCLUDE PXL.Config.inc}

uses
  SysUtils, Classes, PXL.Classes, PXL.TypeDef;

type
  TXMLNode = class;

  TXMLChunk = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FParent: TXMLNode;
  public
    constructor Create(const AParent: TXMLNode);
    destructor Destroy; override;

    property Parent: TXMLNode read FParent;
  end;

  TXMLText = class(TXMLChunk)
  private
    FText: StdString;
  public
    constructor Create(const AParent: TXMLNode; const AText: StdString);

    property Text: StdString read FText;
  end;

  TXMLNode = class(TXMLChunk)
  public type
    PFieldItem = ^TFieldItem;
    TFieldItem = record
      Name: StdString;
      Value: StdString;
    end;

    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FOwner: TXMLNode;
      FIndex: Integer;
      function GetCurrent: TXMLChunk;
    public
      constructor Create(const AOwner: TXMLNode);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: TXMLChunk read GetCurrent;
    end;
  private const
    FieldGrowIncrement = 8;
    FieldGrowFraction = 4;
    ChildGrowIncrement = 5;
    ChildGrowFraction = 5;
    GeneratedXMLTabWidth = 2;
  private
    FName: StdString;

    FFieldItems: array of TFieldItem;
    FFieldItemCount: Integer;

    FFieldSearchList: array of Integer;
    FFieldSearchListDirty: Boolean;

    FChildItems: array of TXMLChunk;
    FChildItemCount: Integer;

    FChildSearchList: array of Integer;
    FChildSearchListDirty: Boolean;

    function GetFieldCount: Integer;
    function GetField(const Index: Integer): PFieldItem;
    procedure RequestFields(const NeedCapacity: Integer);
    procedure InitFieldSearchList;
    procedure FieldSearchListSwap(const Index1, Index2: Integer);
    function FieldSearchListCompare(const Value1, Value2: Integer): Integer;
    function FieldSearchListSplit(const Start, Stop: Integer): Integer;
    procedure FieldSearchListSort(const Start, Stop: Integer);
    procedure UpdateFieldSearchList;
    function GetFieldValue(const FieldName: StdString): StdString;
    procedure SetFieldValue(const FieldName, Value: StdString);

    function GetChildCount: Integer;
    function GetChild(const Index: Integer): TXMLChunk;
    procedure RequestChildren(const NeedCapacity: Integer);
    procedure InitChildSearchList;
    procedure ChildSearchListSwap(const Index1, Index2: Integer);
    function ChildSearchListCompare(const Value1, Value2: Integer): Integer;
    function ChildSearchListSplit(const Start, Stop: Integer): Integer;
    procedure ChildSearchListSort(const Start, Stop: Integer);
    procedure UpdateChildSearchList;
    function GetChildNode(const ChildNodeName: StdString): TXMLNode;

    function GenerateSourceSubCode(const Spacing: Integer = 0): StdString;
  public
    constructor Create(const AName: StdString = ''; const AParent: TXMLNode = nil);
    destructor Destroy; override;

    function GetEnumerator: TEnumerator;

    function AddField(const FieldName, FieldValue: StdString): Integer;
    function IndexOfField(const FieldName: StdString): Integer;

    procedure RemoveField(const Index: Integer);
    procedure ClearFields;

    function AddChildNode(const ChildNodeName: StdString): Integer;
    function AddChildNodeRet(const ChildNodeName: StdString): TXMLNode;

    function AddChildText(const TextContent: StdString): Integer;
    function AddChildTextRet(const TextContent: StdString): TXMLText;

    function IndexOfChild(const ChildName: StdString): Integer;

    procedure RemoveChild(const Index: Integer);
    procedure ClearChildren;

    function GenerateSourceCode: StdString;

    function SaveToFile(const FileName: StdString): Boolean;
    function SaveToStream(const Stream: TStream): Boolean;

    property Name: StdString read FName;

    property FieldCount: Integer read GetFieldCount;
    property Fields[const Index: Integer]: PFieldItem read GetField;
    property FieldValue[const FieldName: StdString]: StdString read GetFieldValue write SetFieldValue;

    property ChildCount: Integer read GetChildCount;
    property Children[const Index: Integer]: TXMLChunk read GetChild;
    property ChildNode[const ChildNodeName: StdString]: TXMLNode read GetChildNode;
  end;

function LoadXMLFromText(const Text: StdString): TXMLNode;
function LoadXMLFromStream(const Stream: TStream): TXMLNode;
function LoadXMLFromFile(const FileName: StdString): TXMLNode;
function LoadXMLFromAsset(const AssetName: StdString): TXMLNode;

implementation

{$REGION 'TXMLChunk'}

constructor TXMLChunk.Create(const AParent: TXMLNode);
begin
  inherited Create;

  Increment_PXL_ClassInstances;
  FParent := AParent;
end;

destructor TXMLChunk.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

{$ENDREGION}
{$REGION 'TXMLText'}

constructor TXMLText.Create(const AParent: TXMLNode; const AText: StdString);
begin
  inherited Create(AParent);

  FText := AText;
end;

{$ENDREGION}
{$REGION 'TXMLNode.TEnumerator'}

constructor TXMLNode.TEnumerator.Create(const AOwner: TXMLNode);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FOwner := AOwner;
  FIndex := -1;
end;

destructor TXMLNode.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TXMLNode.TEnumerator.GetCurrent: TXMLChunk;
begin
  Result := FOwner.Children[FIndex];
end;

function TXMLNode.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FOwner.ChildCount - 1;

  if Result then
    Inc(FIndex);
end;

{$ENDREGION}
{$REGION 'TXMLNode'}

constructor TXMLNode.Create(const AName: StdString; const AParent: TXMLNode);
begin
  inherited Create(AParent);

  FName := AName;
end;

destructor TXMLNode.Destroy;
begin
  ClearChildren;

  inherited;
end;

function TXMLNode.GetFieldCount: Integer;
begin
  Result := FFieldItemCount;
end;

function TXMLNode.GetField(const Index: Integer): PFieldItem;
begin
  if (Index >= 0) and (Index < FFieldItemCount) then
    Result := @FFieldItems[Index]
  else
    Result := nil;
end;

procedure TXMLNode.RequestFields(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FFieldItems);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := FieldGrowIncrement + Capacity + (Capacity div FieldGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := FieldGrowIncrement + NeedCapacity + (NeedCapacity div FieldGrowFraction);

    SetLength(FFieldItems, NewCapacity);
  end;
end;

function TXMLNode.AddField(const FieldName, FieldValue: StdString): Integer;
var
  Index: Integer;
begin
  Index := FFieldItemCount;
  RequestFields(FFieldItemCount + 1);

  FFieldItems[Index].Name := FieldName;
  FFieldItems[Index].Value := FieldValue;

  Inc(FFieldItemCount);
  FFieldSearchListDirty := True;

  Result := Index;
end;

procedure TXMLNode.InitFieldSearchList;
var
  I: Integer;
begin
  if Length(FFieldSearchList) <> FFieldItemCount then
    SetLength(FFieldSearchList, FFieldItemCount);

  for I := 0 to FFieldItemCount - 1 do
    FFieldSearchList[I] := I;
end;

procedure TXMLNode.FieldSearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FFieldSearchList[Index1];
  FFieldSearchList[Index1] := FFieldSearchList[Index2];
  FFieldSearchList[Index2] := TempValue;
end;

function TXMLNode.FieldSearchListCompare(const Value1, Value2: Integer): Integer;
begin
  Result := CompareText(FFieldItems[Value1].Name, FFieldItems[Value2].Name);
end;

function TXMLNode.FieldSearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FFieldSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (FieldSearchListCompare(FFieldSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (FieldSearchListCompare(FFieldSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      FieldSearchListSwap(Left, Right);
  end;

  FieldSearchListSwap(Start, Right);

  Result := Right;
end;

procedure TXMLNode.FieldSearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := FieldSearchListSplit(Start, Stop);

    FieldSearchListSort(Start, SplitPt - 1);
    FieldSearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TXMLNode.UpdateFieldSearchList;
begin
  InitFieldSearchList;

  if FFieldItemCount > 1 then
    FieldSearchListSort(0, FFieldItemCount - 1);

  FFieldSearchListDirty := False;
end;

function TXMLNode.IndexOfField(const FieldName: StdString): Integer;
var
  Lo, Hi, Mid, Res: Integer;
begin
  if FFieldSearchListDirty then
    UpdateFieldSearchList;

  Result := -1;

  Lo := 0;
  Hi := Length(FFieldSearchList) - 1;

  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    Res := CompareText(FFieldItems[FFieldSearchList[Mid]].Name, FieldName);

    if Res = 0 then
    begin
      Result := FFieldSearchList[Mid];
      Break;
    end;

    if Res > 0 then
      Hi := Mid - 1
    else
      Lo := Mid + 1;
  end;
end;

function TXMLNode.GetFieldValue(const FieldName: StdString): StdString;
var
  Index: Integer;
begin
  Index := IndexOfField(FieldName);

  if Index <> -1 then
    Result := FFieldItems[Index].Value
  else
    Result := '';
end;

procedure TXMLNode.SetFieldValue(const FieldName, Value: StdString);
var
  Index: Integer;
begin
  Index := IndexOfField(FieldName);

  if Index <> -1 then
    FFieldItems[Index].Value := Value
  else
    AddField(FieldName, Value);
end;

procedure TXMLNode.RemoveField(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FFieldItemCount) then
    Exit;

  for I := Index to FFieldItemCount - 2 do
    FFieldItems[I] := FFieldItems[I + 1];

  Dec(FFieldItemCount);
  FFieldSearchListDirty := True;
end;

procedure TXMLNode.ClearFields;
begin
  if FFieldItemCount > 0 then
  begin
    FFieldItemCount := 0;
    FFieldSearchListDirty := True;
  end;
end;

function TXMLNode.GetChildCount: Integer;
begin
  Result := FChildItemCount;
end;

function TXMLNode.GetChild(const Index: Integer): TXMLChunk;
begin
  if (Index >= 0) and (Index < FChildItemCount) then
    Result := FChildItems[Index]
  else
    Result := nil;
end;

procedure TXMLNode.RequestChildren(const NeedCapacity: Integer);
var
  NewCapacity, Capacity, I: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FChildItems);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ChildGrowIncrement + Capacity + (Capacity div ChildGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ChildGrowIncrement + NeedCapacity + (NeedCapacity div ChildGrowFraction);

    SetLength(FChildItems, NewCapacity);

    for I := Capacity to NewCapacity - 1 do
      FChildItems[I] := nil;
  end;
end;

function TXMLNode.AddChildNode(const ChildNodeName: StdString): Integer;
var
  Index: Integer;
begin
  Index := FChildItemCount;
  RequestChildren(FChildItemCount + 1);

  FChildItems[Index] := TXMLNode.Create(ChildNodeName, Self);

  Inc(FChildItemCount);
  FChildSearchListDirty := True;

  Result := Index;
end;

function TXMLNode.AddChildNodeRet(const ChildNodeName: StdString): TXMLNode;
var
  Index: Integer;
begin
  Index := AddChildNode(ChildNodeName);

  if Index <> -1 then
    Result := TXMLNode(FChildItems[Index])
  else
    Result := nil;
end;

function TXMLNode.AddChildText(const TextContent: StdString): Integer;
var
  Index: Integer;
begin
  Index := FChildItemCount;
  RequestChildren(FChildItemCount + 1);

  FChildItems[Index] := TXMLText.Create(Self, TextContent);

  Inc(FChildItemCount);
  FChildSearchListDirty := True;

  Result := Index;
end;

function TXMLNode.AddChildTextRet(const TextContent: StdString): TXMLText;
var
  Index: Integer;
begin
  Index := AddChildText(TextContent);

  if Index <> -1 then
    Result := TXMLText(FChildItems[Index])
  else
    Result := nil;
end;

procedure TXMLNode.InitChildSearchList;
var
  I, ChildNodeCount: Integer;
begin
  // Guess initial children count opting for worst/best case scenario that all chunks are nodes.
  if Length(FChildSearchList) <> FChildItemCount then
    SetLength(FChildSearchList, FChildItemCount);

  // Fill search list with child nodes only (text has no name and therefore cannot be searched).
  ChildNodeCount := 0;

  for I := 0 to FChildItemCount - 1 do
    if FChildItems[I] is TXMLNode then
    begin
      FChildSearchList[ChildNodeCount] := I;
      Inc(ChildNodeCount);
    end;

  // Update child list to actual calculated length.
  if Length(FChildSearchList) <> ChildNodeCount then
    SetLength(FChildSearchList, ChildNodeCount);
end;

procedure TXMLNode.ChildSearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FChildSearchList[Index1];
  FChildSearchList[Index1] := FChildSearchList[Index2];
  FChildSearchList[Index2] := TempValue;
end;

function TXMLNode.ChildSearchListCompare(const Value1, Value2: Integer): Integer;
begin
  Result := CompareText(TXMLNode(FChildItems[Value1]).Name, TXMLNode(FChildItems[Value2]).Name);
end;

function TXMLNode.ChildSearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FChildSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (ChildSearchListCompare(FChildSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (ChildSearchListCompare(FChildSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      ChildSearchListSwap(Left, Right);
  end;

  ChildSearchListSwap(Start, Right);

  Result := Right;
end;

procedure TXMLNode.ChildSearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := ChildSearchListSplit(Start, Stop);

    ChildSearchListSort(Start, SplitPt - 1);
    ChildSearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TXMLNode.UpdateChildSearchList;
begin
  InitChildSearchList;

  if Length(FChildSearchList) > 1 then
    ChildSearchListSort(0, Length(FChildSearchList) - 1);

  FChildSearchListDirty := False;
end;

function TXMLNode.IndexOfChild(const ChildName: StdString): Integer;
var
  Lo, Hi, Mid, Res: Integer;
begin
  if FChildSearchListDirty then
    UpdateChildSearchList;

  Result := -1;

  Lo := 0;
  Hi := Length(FChildSearchList) - 1;

  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    Res := CompareText(TXMLNode(FChildItems[FChildSearchList[Mid]]).Name, ChildName);

    if Res = 0 then
    begin
      Result := FChildSearchList[Mid];
      Break;
    end;

    if Res > 0 then
      Hi := Mid - 1
    else
      Lo := Mid + 1;
  end;
end;

function TXMLNode.GetChildNode(const ChildNodeName: StdString): TXMLNode;
var
  Index: Integer;
begin
  Index := IndexOfChild(ChildNodeName);

  if Index <> -1 then
    Result := TXMLNode(FChildItems[Index])
  else
    Result := nil;
end;

procedure TXMLNode.RemoveChild(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FChildItemCount) then
    Exit;

  FChildItems[Index].Free;

  for I := Index to FChildItemCount - 2 do
    FChildItems[I] := FChildItems[I + 1];

  Dec(FChildItemCount);
  FChildSearchListDirty := True;
end;

procedure TXMLNode.ClearChildren;
var
  I: Integer;
begin
  if FChildItemCount > 0 then
  begin
    for I := FChildItemCount - 1 downto 0 do
      FChildItems[I].Free;

    FChildItemCount := 0;
    FChildSearchListDirty := True;
  end;
end;

function TXMLNode.GenerateSourceSubCode(const Spacing: Integer): StdString;

  function AddSpaces(const Count: Integer): StdString;
  var
    I: Integer;
  begin
    SetLength(Result, Count);

    for I := 0 to Count - 1 do
      Result[1 + I] := ' ';
  end;

var
  I: Integer;
begin
  Result := AddSpaces(Spacing) + '<' + FName;

  if FFieldItemCount > 0 then
  begin
    Result := Result + ' ';

    for I := 0 to FFieldItemCount - 1 do
    begin
      Result := Result + FFieldItems[I].Name + '="' + FFieldItems[I].Value + '"';

      if I < FFieldItemCount - 1 then
        Result := Result + ' ';
    end;
  end;

  if FChildItemCount > 0 then
  begin
    Result := Result + '>'#13#10;

    for I := 0 to FChildItemCount - 1 do
      if FChildItems[I] is TXMLNode then
        Result := Result + TXMLNode(FChildItems[I]).GenerateSourceSubCode(Spacing + GeneratedXMLTabWidth)
      else if FChildItems[I] is TXMLText then
        Result := Result + AddSpaces(Spacing + GeneratedXMLTabWidth) + TXMLText(FChildItems[I]).Text + #13#10;

    Result := Result + AddSpaces(Spacing) + '</' + FName + '>'#13#10;
  end
  else
    Result := Result + ' />'#13#10;
end;

function TXMLNode.GenerateSourceCode: StdString;
begin
  Result := GenerateSourceSubCode;
end;

function TXMLNode.SaveToFile(const FileName: StdString): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    try
      Result := SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Exit(False);
  end;
end;

function TXMLNode.SaveToStream(const Stream: TStream): Boolean;
var
  StStream: TStringStream;
begin
  try
    StStream := TStringStream.Create(GenerateSourceCode);
    try
      Stream.CopyFrom(StStream, 0);
      Result := True;
    finally
      StStream.Free;
    end;
  except
    Exit(False);
  end;
end;

function TXMLNode.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'XML Parser'}

const
  XWhiteSpace = [#0, #9, #10, #13, #32];
  XQuotes = ['"', ''''];
  XSeparators = ['+', '-', '=', '<', '>', '(', ')', '[', ']', '"', '''', ',', '.', '/', '\', ':', ';', '*', '#', '&',
    '@', '$', '%', '^', '?', '!'];
  XNameSpecial = [':', '-'];
  XParsingOkay = 0;
  XIncompleteAttribute = -1;
  XInvalidNodeDeclaration = -2;
  XInvalidDocumentSymbol = -3;
  XInvalidClosingTagName = -4;
  XInvalidClosingTagSyntax = -5;
  XNodeHasNotBeenClosed = -6;

var
  CurrentParseStatus: Integer = 0;

function IsNameCharacter(const ScanCh: Char): Boolean; inline;
begin
  Result := (not (ScanCh in XWhiteSpace)) and (not (ScanCh in XSeparators));
end;

function IsNameCharacterSpecial(const ScanCh: Char): Boolean; inline;
begin
  Result := IsNameCharacter(ScanCh) or (ScanCh in XNameSpecial);
end;

procedure SkipBlankSpace(const Text: StdString; var TextPos: Integer);
begin
  while (TextPos <= Length(Text)) and (Text[TextPos] in XWhiteSpace) do
    Inc(TextPos);
end;

function HasTextPortion(const Text: StdString; const TextPos: Integer; const SubText: StdString): Boolean;
var
  TestText: StdString;
begin
  if 1 + Length(Text) - TextPos < Length(SubText) then
    Exit(False);

  TestText := Copy(Text, TextPos, Length(SubText));
  Result := SameText(TestText, SubText);
end;

procedure ScanAfterPortion(const Text: StdString; var TextPos: Integer; const SubText: StdString);
begin
  while TextPos <= Length(Text) do
  begin
    if HasTextPortion(Text, TextPos, SubText) then
    begin
      Inc(TextPos, Length(SubText));
      Break;
    end;

    Inc(TextPos);
  end;
end;

procedure ScanAfterChar(const Text: StdString; var TextPos: Integer; const ScanCh: Char);
begin
  while TextPos <= Length(Text) do
  begin
    if Text[TextPos] = ScanCh then
    begin
      Inc(TextPos);
      Break;
    end;

    Inc(TextPos);
  end;
end;

function ScanForName(const Text: StdString; var TextPos: Integer): StdString;
var
  StartPos, CopyLen: Integer;
  ValidChar: Boolean;
begin
  Result := '';

  SkipBlankSpace(Text, TextPos);

  if (TextPos > Length(Text)) or (Text[TextPos] in XSeparators) then
    Exit;

  StartPos := TextPos;
  CopyLen := 0;

  while TextPos <= Length(Text) do
  begin
    if CopyLen > 0 then
      ValidChar := IsNameCharacterSpecial(Text[TextPos])
    else
      ValidChar := IsNameCharacter(Text[TextPos]);

    if not ValidChar then
      Break;

    Inc(CopyLen);
    Inc(TextPos);
  end;

  Result := Copy(Text, StartPos, CopyLen);
end;

function ScanForTextValue(const Text: StdString; var TextPos: Integer): StdString;
var
  StartPos, CopyLen: Integer;
  QuoteCh: Char;
begin
  Result := '';

  SkipBlankSpace(Text, TextPos);

  if (TextPos > Length(Text)) or ((Text[TextPos] in XSeparators) and (not (Text[TextPos] in XQuotes))) then
    Exit;

  // Opening quote?
  if Text[TextPos] in XQuotes then
  begin
    QuoteCh := Text[TextPos];
    Inc(TextPos);
  end
  else
    QuoteCh := #0;

  StartPos := TextPos;
  CopyLen := 0;

  while TextPos <= Length(Text) do
  begin
    // Closing quote.
    if (QuoteCh <> #0) and (Text[TextPos] = QuoteCh) then
    begin
      Inc(TextPos);
      Break;
    end;

    if (QuoteCh = #0) and (not IsNameCharacter(Text[TextPos])) then
      Break;

    Inc(CopyLen);
    Inc(TextPos);
  end;

  Result := Copy(Text, StartPos, CopyLen);
end;

procedure ParseXMLField(const Node: TXMLNode; const Text: StdString; var TextPos: Integer);
var
  FieldName, FieldValue: StdString;
begin
  FieldName := ScanForName(Text, TextPos);

  // Skip any blank space.
  SkipBlankSpace(Text, TextPos);

  // Abrupt end of text?
  if TextPos > Length(Text) then
  begin
    if Length(FieldName) > 0 then
      Node.AddField(FieldName, '');

    CurrentParseStatus := XIncompleteAttribute;
    Exit;
  end;

  // Field has no value.
  if Text[TextPos] <> '=' then
  begin
    if Length(FieldName) > 0 then
      Node.AddField(FieldName, '');

    Exit;
  end;

  // Parse field value (skip "=" symbol).
  Inc(TextPos);
  FieldValue := ScanForTextValue(Text, TextPos);

  if Length(FieldName) > 0 then
    Node.AddField(FieldName, FieldValue);
end;

procedure ResetTextContentScan(const TextPos: Integer; out TextBlockStart, TextBlockEnd: Integer); inline;
begin
  TextBlockStart := TextPos;
  TextBlockEnd := TextPos;
end;

procedure CheckForTextContent(const Node: TXMLNode; const Text: StdString; const TextPos: Integer; var TextBlockStart,
  TextBlockEnd: Integer);
var
  TextContent: StdString;
begin
  if TextBlockEnd > TextBlockStart then
  begin
    TextContent := Trim(Copy(Text, TextBlockStart, TextBlockEnd - TextBlockStart));
    if Length(TextContent) > 0 then
      Node.AddChildText(TextContent);

    ResetTextContentScan(TextPos, TextBlockStart, TextBlockEnd);
  end;
end;

function ParseXMLNode(const Root: TXMLNode; const Text: StdString; var TextPos: Integer): TXMLNode;
var
  NodeName, TextContent: StdString;
  TextBlockStart, TextBlockEnd: Integer;
begin
  // Process node name.
  NodeName := ScanForName(Text, TextPos);

  if Root <> nil then
    Result := Root.AddChildNodeRet(NodeName)
  else
    Result := TXMLNode.Create(NodeName);

  // Processing after [<NODE]...
  while TextPos <= Length(Text) do
  begin
    // Skip any blank space.
    SkipBlankSpace(Text, TextPos);
    if TextPos > Length(Text) then
      Break;

    // Skip "<!-- ... -->" comments inside node (is this allowed?)
    if HasTextPortion(Text, TextPos, '<!--') then
    begin
      Inc(TextPos, 4);
      ScanAfterPortion(Text, TextPos, '-->');
      Continue;
    end;

    // Full end of node.
    if HasTextPortion(Text, TextPos, '/>') then
    begin
      Inc(TextPos, 2);
      Exit;
    end;

    // End of node, need to parse the second part.
    if Text[TextPos] = '>' then
    begin
      Inc(TextPos);
      Break;
    end;

    // Attribute.
    if IsNameCharacter(Text[TextPos]) then
    begin
      ParseXMLField(Result, Text, TextPos);
      Continue;
    end;

    CurrentParseStatus := XInvalidNodeDeclaration;
    Exit;
  end;

  ResetTextContentScan(TextPos, TextBlockStart, TextBlockEnd);

  // Processing after [<NODE>]...
  while TextPos <= Length(Text) do
  begin
    // Process text block if it was previously detected.
    CheckForTextContent(Result, Text, TextPos, TextBlockStart, TextBlockEnd);

    // Skip any blank space.
    SkipBlankSpace(Text, TextPos);
    if TextPos > Length(Text) then
      Break;

    // Skip "<!-- ... -->" comments.
    if HasTextPortion(Text, TextPos, '<!--') then
    begin
      TextBlockEnd := TextPos;
      Inc(TextPos, 4);

      ScanAfterPortion(Text, TextPos, '-->');
      ResetTextContentScan(TextPos, TextBlockStart, TextBlockEnd);

      Continue;
    end;

    // Skip "<? ... ?>" tags.
    if HasTextPortion(Text, TextPos, '<?') then
    begin
      TextBlockEnd := TextPos;
      Inc(TextPos, 2);

      ScanAfterPortion(Text, TextPos, '?>');
      ResetTextContentScan(TextPos, TextBlockStart, TextBlockEnd);

      Continue;
    end;

    // Skip "<!doctype >" tags.
    if HasTextPortion(Text, TextPos, '<!doctype') then
    begin
      TextBlockEnd := TextPos;
      Inc(TextPos, 9);

      ScanAfterChar(Text, TextPos, '>');
      ResetTextContentScan(TextPos, TextBlockStart, TextBlockEnd);

      Continue;
    end;

    // End of node "</NODE>"
    if HasTextPortion(Text, TextPos, '</') then
    begin
      TextBlockEnd := TextPos;
      CheckForTextContent(Result, Text, TextPos, TextBlockStart, TextBlockEnd);

      Inc(TextPos, 2);

      NodeName := ScanForName(Text, TextPos);
      if not SameText(NodeName, Result.Name) then
      begin
        CurrentParseStatus := XInvalidClosingTagName;
        Exit;
      end;

      SkipBlankSpace(Text, TextPos);
      if (TextPos > Length(Text)) or (Text[TextPos] <> '>') then
      begin
        CurrentParseStatus := XInvalidClosingTagSyntax;
        Exit;
      end;

      Inc(TextPos);
      Exit;
    end;

    // Start of child node.
    if Text[TextPos] = '<' then
    begin
      TextBlockEnd := TextPos;
      CheckForTextContent(Result, Text, TextPos, TextBlockStart, TextBlockEnd);

      Inc(TextPos);

      ParseXMLNode(Result, Text, TextPos);
      ResetTextContentScan(TextPos, TextBlockStart, TextBlockEnd);

      Continue;
    end;

    // Skip text inside the node.
    Inc(TextPos);
  end;

  if TextPos > Length(Text) then
    CurrentParseStatus := XNodeHasNotBeenClosed;
end;

function LoadXMLFromText(const Text: StdString): TXMLNode;
var
  TextPos: Integer;
begin
  Result := nil;
  if Length(Text) < 1 then
    Exit;

  TextPos := 1;
  CurrentParseStatus := XParsingOkay;

  while TextPos <= Length(Text) do
  begin
    // Skip any blank space.
    SkipBlankSpace(Text, TextPos);
    if TextPos > Length(Text) then
      Break;

    // Skip "<!-- ... -->" comments.
    if HasTextPortion(Text, TextPos, '<!--') then
    begin
      Inc(TextPos, 4);
      ScanAfterPortion(Text, TextPos, '-->');
      Continue;
    end;

    // Skip "<? ... ?>" tags.
    if HasTextPortion(Text, TextPos, '<?') then
    begin
      Inc(TextPos, 2);
      ScanAfterPortion(Text, TextPos, '?>');
      Continue;
    end;

    // Skip "<!doctype >" tags.
    if HasTextPortion(Text, TextPos, '<!doctype') then
    begin
      Inc(TextPos, 9);
      ScanAfterChar(Text, TextPos, '>');
      Continue;
    end;

    // Start of node.
    if Text[TextPos] = '<' then
    begin
      Inc(TextPos);

      Result := ParseXMLNode(nil, Text, TextPos);
      Break;
    end;

    // Invalid text character.
    CurrentParseStatus := XInvalidDocumentSymbol;
    Break;
  end;

  if CurrentParseStatus <> XParsingOkay then
    if Result <> nil then
      FreeAndNil(Result);
end;

{$ENDREGION}
{$REGION 'XML Loading Functions'}

function LoadXMLFromStream(const Stream: TStream): TXMLNode;
var
  StStream: TStringStream;
begin
  try
    StStream := TStringStream.Create('');
    try
      StStream.CopyFrom(Stream, 0);
      Result := LoadXMLFromText(StStream.DataString);
    finally
      StStream.Free;
    end;
  except
    Exit(nil);
  end;
end;

function LoadXMLFromFile(const FileName: StdString): TXMLNode;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadXMLFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Exit(nil);
  end;
end;

function LoadXMLFromAsset(const AssetName: StdString): TXMLNode;
var
  Stream: TAssetStream;
begin
  try
    Stream := TAssetStream.Create(AssetName);
    try
      Result := LoadXMLFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Exit(nil);
  end;
end;

{$ENDREGION}

end.
