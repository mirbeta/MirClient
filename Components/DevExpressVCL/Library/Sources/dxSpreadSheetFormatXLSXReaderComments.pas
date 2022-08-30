{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetFormatXLSXReaderComments;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, dxCore, dxCoreClasses, cxClasses, dxCustomTree, dxXMLDoc, dxZIPUtils,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetStrs, dxSpreadSheetPackedFileFormatCore,
  dxSpreadSheetUtils, dxGDIPlusClasses, Generics.Defaults, Generics.Collections, dxCoreGraphics, cxGeometry, dxHashUtils, Variants,
  dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetFormatXLSXReader;

type
  TdxSpreadSheetXLSXReaderCommentContainersParser = class;

  { TdxSpreadSheetXLSXReaderCommentsParser }

  TdxSpreadSheetXLSXReaderCommentsParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  strict private
    FView: TdxSpreadSheetTableView;
  protected
    procedure ProcessComment(ANode: TdxXMLNode; AUserData: Pointer);
    function ReadAuthor(AAuthorID: Integer): string;
  public
    constructor Create(const AFileName: AnsiString; AView: TdxSpreadSheetTableView; AOwner: TdxSpreadSheetXLSXReader); reintroduce;
    procedure Execute; override;
    //
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetXLSXReaderCommentContainerParser }

  TdxSpreadSheetXLSXReaderCommentContainerParser = class(TdxSpreadSheetXLSXReaderCustomDocumentSubParser)
  strict private
    FColorNamesMap: TDictionary<string, string>;
    FContainer: TdxSpreadSheetCommentContainer;

    function GetNodeValueAsBoolean(ANode: TdxXMLNode;
      ADefaultValueForEmptyValue, ADefaultValueForNonExistsNode: Boolean): Boolean; overload;
    function GetNodeValueAsBoolean(AParentNode: TdxXMLNode; const ANode: TdxXMLString;
      ADefaultValueForEmptyValue, ADefaultValueForNonExistsNode: Boolean): Boolean; overload;
    function SplitString(const S, ASplitter: string): TStringList;
  protected
    function DecodeColor(S: string; ADefaultColor: TColor = clBlack): TColor;
    function DecodeOpacity(const S: string): Byte;

    function ReadAlphaColor(ANode: TdxXMLNode; const AAttrName: TdxXMLString;
      const AOpacityAttrName: TdxXMLString = ''; AOpacityNode: TdxXMLNode = nil;
      const ADefaultColor: TColor = clBlack): TdxAlphaColor;
    function ReadGradientAngle(ANode: TdxXMLNode): Double;

    procedure ReadAnchorPoints(ANode: TdxXMLNode); virtual;
    procedure ReadClientData(ANode: TdxXMLNode); virtual;
    procedure ReadClientDataAnchors(ANode: TdxXMLNode); virtual;
    procedure ReadClientDataTextBox(ANode: TdxXMLNode); virtual;
    procedure ReadShapeFill(AFill: TdxGPBrush); virtual;
    procedure ReadShapeStroke(AStroke: TdxGPPen); virtual;
    procedure ReadTextBox(ANode: TdxXMLNode); virtual;
    procedure ReadTextBoxInset(const AInset: string); virtual;
    procedure ReadTextBoxStyles(const AStyles: string); virtual;
  public
    constructor Create(ANode: TdxXMLNode;
      AOwnerParser: TdxSpreadSheetXLSXReaderCommentContainersParser;
      AContainer: TdxSpreadSheetCommentContainer); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    //
    property Container: TdxSpreadSheetCommentContainer read FContainer;
  end;

  { TdxSpreadSheetXLSXReaderCommentContainersParser }

  TdxSpreadSheetXLSXReaderCommentContainersParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  strict private
    FView: TdxSpreadSheetTableView;

    procedure ProcessSubNodes(ANode: TdxXMLNode; AUserData: Pointer);
  protected
    procedure ProcessShape(ANode: TdxXMLNode; AUserData: Pointer);
  public
    constructor Create(const AFileName: AnsiString; AView: TdxSpreadSheetTableView; AOwner: TdxSpreadSheetXLSXReader); reintroduce;
    procedure Execute; override;
    //
    property View: TdxSpreadSheetTableView read FView;
  end;

implementation

uses
  dxSpreadSheetFormatXLSXTags, dxSpreadSheetFormatUtils, Math, dxSpreadSheetFormatXLSXReaderDrawing,
  dxSpreadSheetCoreStyles;

type
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);

{ TdxSpreadSheetXLSXReaderCommentsParser }

constructor TdxSpreadSheetXLSXReaderCommentsParser.Create(const AFileName: AnsiString;
  AView: TdxSpreadSheetTableView; AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create(AFileName, AOwner);
  FView := AView;
end;

procedure TdxSpreadSheetXLSXReaderCommentsParser.Execute;
var
  ANode: TdxXMLNode;
begin
  if Document.FindChild([sdxXLSXNodeComments, sdxXLSXNodeCommentList], ANode) then
    ANode.ForEach(ProcessComment);
end;

procedure TdxSpreadSheetXLSXReaderCommentsParser.ProcessComment(ANode: TdxXMLNode; AUserData: Pointer);
var
  ACellRef: TPoint;
  AContainer: TdxSpreadSheetCommentContainer;
  AString: TdxSpreadSheetFormattedSharedString;
  ATextNode: TdxXMLNode;
begin
  dxStringToReference(ANode.Attributes.GetValueAsString(sdxXLSXAttrRef), ACellRef.X, ACellRef.Y);
  AContainer := TdxSpreadSheetCommentContainer(View.Containers.AddCommentContainer(View.CreateCell(ACellRef.Y, ACellRef.X)));
  AContainer.Author := ReadAuthor(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrAuthorId, -1));
  AContainer.Visible := False;
  if ANode.FindChild(sdxXLSXNodeTextFull, ATextNode) then
  begin
    AString := TdxSpreadSheetXLSXReaderRichTextParser.Parse(Owner, ATextNode);
    AContainer.TextBox.Text := AString;
    if AString.Runs.Count > 0 then
      AContainer.TextBox.Font.Handle := AString.Runs[0].FontHandle;
  end;
end;

function TdxSpreadSheetXLSXReaderCommentsParser.ReadAuthor(AAuthorID: Integer): string;
var
  ANode: TdxXMLNode;
begin
  Result := '';
  if Document.FindChild([sdxXLSXNodeComments, sdxXLSXNodeAuthors], ANode) then
  begin
    if (AAuthorID >= 0) and (AAuthorID < ANode.Count) then
      Result := ANode.Items[AAuthorID].TextAsString;
  end;
end;

{ TdxSpreadSheetXLSXReaderCommentContainerParser }

constructor TdxSpreadSheetXLSXReaderCommentContainerParser.Create(ANode: TdxXMLNode;
  AOwnerParser: TdxSpreadSheetXLSXReaderCommentContainersParser; AContainer: TdxSpreadSheetCommentContainer);
begin
  inherited Create(ANode, AOwnerParser);
  FContainer := AContainer;
  FColorNamesMap := TDictionary<string, string>.Create;
  FColorNamesMap.Add('infoBackground', 'InfoBK');
end;

destructor TdxSpreadSheetXLSXReaderCommentContainerParser.Destroy;
begin
  FreeAndNil(FColorNamesMap);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.Execute;
var
  ASubNode: TdxXMLNode;
begin
  ReadShapeStroke(Container.Shape.Pen);
  ReadShapeFill(Container.Shape.Brush);
  if Node.FindChild(sdxXLSXNodeXClientData, ASubNode) then
    ReadClientData(ASubNode);
  if Node.FindChild(sdxXLSXNodeVMLTextBox, ASubNode) then
    ReadTextBox(ASubNode);
end;

function TdxSpreadSheetXLSXReaderCommentContainerParser.DecodeColor(S: string; ADefaultColor: TColor = clBlack): TColor;
var
  AValue: string;
begin
  if Pos(' ', S) > 0 then
    Delete(S, Pos(' ', S), MaxInt);

  if (S <> '') and (S[1] = '#') then
    Result := inherited DecodeColor(Copy(S, 2, MaxInt))
  else
  begin
    if FColorNamesMap.TryGetValue(S, AValue) then
      S := AValue;
    if not IdentToColor('cl' + S, Integer(Result))  then
      Result := ADefaultColor;
  end;
end;

function TdxSpreadSheetXLSXReaderCommentContainerParser.DecodeOpacity(const S: string): Byte;
var
  ALength: Integer;
begin
  ALength := Length(S);
  if ALength = 0 then
    Result := MaxByte
  else
    case S[ALength] of
      'f':
        Result := MulDiv(MaxByte, StrToInt(Copy(S, 1, ALength - 1)), MaxWord);
      '%':
        Result := MulDiv(MaxByte, StrToInt(Copy(S, 1, ALength - 1)), 100);
    else
      Result := StrToIntDef(S, MaxByte);
    end;
end;

function TdxSpreadSheetXLSXReaderCommentContainerParser.ReadAlphaColor(ANode: TdxXMLNode;
  const AAttrName: TdxXMLString; const AOpacityAttrName: TdxXMLString = ''; AOpacityNode: TdxXMLNode = nil;
  const ADefaultColor: TColor = clBlack): TdxAlphaColor;
var
  AOpacity: Byte;
begin
  AOpacity := MaxByte;
  if AOpacityAttrName <> '' then
  begin
    if AOpacityNode = nil then
      AOpacityNode := ANode;
    AOpacity := DecodeOpacity(AOpacityNode.Attributes.GetValueAsString(AOpacityAttrName));
  end;
  Result := dxColorToAlphaColor(DecodeColor(ANode.Attributes.GetValueAsString(AAttrName), ADefaultColor), AOpacity);
end;

function TdxSpreadSheetXLSXReaderCommentContainerParser.ReadGradientAngle(ANode: TdxXMLNode): Double;
var
  AFocus: Integer;
begin
  Result := dxNormalizeAngle(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrAngle));
  AFocus := StrToIntDef(StringReplace(ANode.Attributes.GetValueAsString(sdxXLSXAttrFocus), '%', '', [rfReplaceAll]), 0);

  case Round(Result / 45) of
    0, 2, 4, 6, 8:
      Result := Result + 90;
    3, 7:
      AFocus := 100 - AFocus;
  end;

  if AFocus >= 50 then
    Result := Result + 180;
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadAnchorPoints(ANode: TdxXMLNode);

  function GetCell(const ARow, AColumn: string): TdxSpreadSheetCell;
  begin
    Result := TdxSpreadSheetTableView(Container.Parent).CreateCell(StrToInt(ARow), StrToInt(AColumn));
  end;

  function GetOffset(const ARowOffset, AColumnOffset: string): TPoint;
  begin
    Result := Point(StrToIntDef(AColumnOffset, 0), StrToIntDef(ARowOffset, 0));
  end;

var
  AList: TStringList;
begin

  AList := SplitString(ANode.TextAsString, ',');
  try
    if AList.Count = 8 then
    begin
      Container.BeginUpdate;
      try
        Container.AnchorType := catTwoCell;
        Container.AnchorPoint1.Cell := GetCell(AList[2], AList[0]);
        Container.AnchorPoint1.Offset := GetOffset(AList[3], AList[1]);
        Container.AnchorPoint2.Cell := GetCell(AList[6], AList[4]);
        Container.AnchorPoint2.Offset := GetOffset(AList[7], AList[5]);
      finally
        Container.EndUpdate;
      end;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadClientData(ANode: TdxXMLNode);
begin
  ReadClientDataAnchors(ANode);
  ReadClientDataTextBox(ANode);

  Container.Visible := GetNodeValueAsBoolean(ANode, sdxXLSXNodeXVisible, True, False);
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadClientDataAnchors(ANode: TdxXMLNode);

  function ReadAnchorType(ANode: TdxXMLNode): TdxSpreadSheetContainerAnchorType;
  begin
    if GetNodeValueAsBoolean(ANode, sdxXLSXNodeXMoveWithCells, False, True) then
    begin
      if GetNodeValueAsBoolean(ANode, sdxXLSXNodeXSizeWithCells, False, True) then
        Result := catTwoCell
      else
        Result := catOneCell;
    end
    else
      Result := catAbsolute;
  end;

var
  ASavedBounds: TRect;
  ASubNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxXLSXNodeXAnchor, ASubNode) then
    ReadAnchorPoints(ASubNode);

  ASavedBounds := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;
  Container.AnchorType := ReadAnchorType(ANode);
  TdxSpreadSheetContainerAccess(Container).Calculator.UpdateAnchors(ASavedBounds);
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadClientDataTextBox(ANode: TdxXMLNode);
begin
  Container.TextBox.AlignHorz := TdxSpreadSheetXLSXHelper.StringToVMLTextAlignHorz(ANode.ChildValues[sdxXLSXNodeXTextHAlign]);
  Container.TextBox.AlignVert := TdxSpreadSheetXLSXHelper.StringToVMLTextAlignVert(ANode.ChildValues[sdxXLSXNodeXTextVAlign]);
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadShapeFill(AFill: TdxGPBrush);
var
  AInvertOrder: Boolean;
  AStream: TStream;
  ASubNode: TdxXMLNode;
begin
  AFill.Style := gpbsSolid;
  AFill.Color := ReadAlphaColor(Node, sdxXLSXAttrFillColor, '', nil, clInfoBk);
  if Node.FindChild(sdxXLSXNodeVMLFill, ASubNode) then
  begin
    if ASubNode.Attributes.GetValue(sdxXLSXAttrTypeLC) = sdxXLSXValueGradient then
    begin
      AFill.Style := gpbsGradient;
      AFill.GradientPoints.Add(0, ReadAlphaColor(Node, sdxXLSXAttrFillColor, sdxXLSXAttrOpacity, ASubNode));
      AFill.GradientPoints.Add(1, ReadAlphaColor(ASubNode, sdxXLSXAttrColor2, sdxXLSXAttrOOpacity2));
      AFill.GradientMode := dxGetNearestGradientMode(ReadGradientAngle(ASubNode), AInvertOrder);
      if AInvertOrder then
        AFill.GradientPoints.InvertOrder;
    end
    else

    if ASubNode.Attributes.GetValue(sdxXLSXAttrTypeLC) = sdxXLSXValueTile then
    begin
      if ReadEmbeddedImage(ASubNode.Attributes.GetValue(sdxXLSXAttrORelID), AStream) then
      begin
        AFill.Style := gpbsTexture;
        AFill.Texture.LoadFromStream(AStream);
      end;
    end
    else

    if ASubNode.Attributes.GetValue(sdxXLSXAttrTypeLC) = sdxXLSXValuePattern then
    begin
      if ReadEmbeddedImage(ASubNode.Attributes.GetValue(sdxXLSXAttrORelID), AStream) then
      begin
        AFill.Texture.LoadFromStream(AStream);
        dxSpreadSheetInitializeBrushPattern(AFill, AFill.Texture,
          ReadAlphaColor(ASubNode, sdxXLSXAttrColor2, sdxXLSXAttrOOpacity2),
          ReadAlphaColor(Node, sdxXLSXAttrFillColor, sdxXLSXAttrOpacity, ASubNode));
      end;
    end
    else
    begin
      AFill.Style := gpbsSolid;
      AFill.Color := ReadAlphaColor(Node, sdxXLSXAttrFillColor, sdxXLSXAttrOpacity, ASubNode, clInfoBk);
    end;
  end;
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadShapeStroke(AStroke: TdxGPPen);
var
  AAttr: TdxXMLNodeAttribute;
  ASubNode: TdxXMLNode;
begin
  AStroke.Brush.Style := gpbsSolid;
  AStroke.Brush.Color := ReadAlphaColor(Node, sdxXLSXAttrStrokeColor);
  if Node.Attributes.Find(sdxXLSXAttrStrokeWeight, AAttr) then
    AStroke.Width := TdxValueUnitsHelper.ValueToPixels(AAttr.ValueAsString);
  if Node.FindChild(sdxXLSXNodeVMLStroke, ASubNode) then
    AStroke.Style := TdxSpreadSheetXLSXHelper.StringToVMLDashStyle(ASubNode.Attributes.GetValue(sdxXLSXAttrDashStyle));
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadTextBox(ANode: TdxXMLNode);
begin
  if Node.Attributes.GetValue(sdxXLSXAttrOInsetMode) = 'auto' then
    Container.TextBox.ContentOffsets := dxEMUToPixels(dxXLSXDefaultTextPadding)
  else
    ReadTextBoxInset(ANode.Attributes.GetValueAsString(sdxXLSXAttrInset));

  ReadTextBoxStyles(ANode.Attributes.GetValueAsString(sdxXLSXAttrStyle));
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadTextBoxInset(const AInset: string);
var
  AContentOffsets: TRect;
  AList: TStringList;
begin
  AList := SplitString(AInset, ',');
  try
    AContentOffsets := Container.TextBox.ContentOffsets;
    if AList.Count > 0 then
      AContentOffsets.Left := TdxValueUnitsHelper.ValueToPixels(AList[0]);
    if AList.Count > 1 then
      AContentOffsets.Top := TdxValueUnitsHelper.ValueToPixels(AList[1]);
    if AList.Count > 2 then
      AContentOffsets.Right := TdxValueUnitsHelper.ValueToPixels(AList[2]);
    if AList.Count > 3 then
      AContentOffsets.Bottom := TdxValueUnitsHelper.ValueToPixels(AList[3]);
    Container.TextBox.ContentOffsets := AContentOffsets;
  finally
    AList.Free;
  end;
end;

procedure TdxSpreadSheetXLSXReaderCommentContainerParser.ReadTextBoxStyles(const AStyles: string);
var
  AStyleList: TStringList;
begin
  AStyleList := SplitString(AStyles, ';');
  try
    AStyleList.NameValueSeparator := ':';
    Container.TextBox.AutoSize := Trim(AStyleList.Values[sdxXLSXMSOFitShapeToText]) = 't';
  finally
    AStyleList.Free;
  end;
end;

function TdxSpreadSheetXLSXReaderCommentContainerParser.GetNodeValueAsBoolean(
  ANode: TdxXMLNode; ADefaultValueForEmptyValue, ADefaultValueForNonExistsNode: Boolean): Boolean;
begin
  if ANode = nil then
    Result := ADefaultValueForNonExistsNode
  else
    if ANode.Text <> '' then
      Result := ANode.TextAsBoolean
    else
      Result := ADefaultValueForEmptyValue;
end;

function TdxSpreadSheetXLSXReaderCommentContainerParser.GetNodeValueAsBoolean(AParentNode: TdxXMLNode;
  const ANode: TdxXMLString; ADefaultValueForEmptyValue, ADefaultValueForNonExistsNode: Boolean): Boolean;
begin
  Result := GetNodeValueAsBoolean(AParentNode.FindChild(ANode), ADefaultValueForEmptyValue, ADefaultValueForNonExistsNode);
end;

function TdxSpreadSheetXLSXReaderCommentContainerParser.SplitString(const S, ASplitter: string): TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Text := StringReplace(S, ASplitter, #10, [rfReplaceAll]);
  for I := 0 to Result.Count - 1 do
    Result[I] := Trim(Result[I]);
end;

{ TdxSpreadSheetXLSXReaderCommentContainersParser }

constructor TdxSpreadSheetXLSXReaderCommentContainersParser.Create(
  const AFileName: AnsiString; AView: TdxSpreadSheetTableView; AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create(AFileName, AOwner);
  FView := AView;
end;

procedure TdxSpreadSheetXLSXReaderCommentContainersParser.Execute;
var
  ANode: TdxXMLNode;
begin
  if Document.FindChild(sdxXLSXNodeXML, ANode) then
    ANode.ForEach(ProcessSubNodes);
end;

procedure TdxSpreadSheetXLSXReaderCommentContainersParser.ProcessShape(ANode: TdxXMLNode; AUserData: Pointer);
var
  ACellRef: TPoint;
  AContainer: TdxSpreadSheetCommentContainer;
  ASubNode: TdxXMLNode;
begin
  ACellRef := cxInvalidPoint;
  if ANode.FindChild([sdxXLSXNodeXClientData, sdxXLSXNodeXColumn], ASubNode) then
    ACellRef.X := StrToIntDef(ASubNode.TextAsString, -1);
  if ANode.FindChild([sdxXLSXNodeXClientData, sdxXLSXNodeXRow], ASubNode) then
    ACellRef.Y := StrToIntDef(ASubNode.TextAsString, -1);
  if View.Containers.FindCommentContainer(ACellRef.Y, ACellRef.X, AContainer) then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderCommentContainerParser.Create(ANode, Self, AContainer));
end;

procedure TdxSpreadSheetXLSXReaderCommentContainersParser.ProcessSubNodes(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxXLSXNodeVMLShape then
    ProcessShape(ANode, AUserData);
end;

end.
