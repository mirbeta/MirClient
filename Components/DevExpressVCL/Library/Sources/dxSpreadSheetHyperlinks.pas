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

unit dxSpreadSheetHyperlinks;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Math, Types, SysUtils, Classes, Controls, cxClasses, dxCore, cxVariants, dxSpreadSheetUtils, dxCoreClasses,
  dxHashUtils;

type
  EdxSpreadSheetHyperlinkExecuteError = class(EdxException);

  TdxSpreadSheetHyperlink = class;
  TdxSpreadSheetHyperlinks = class;

  TdxSpreadSheetHyperlinkValueType = (hvtFileOrWebPage, hvtReference, hvtEMail);

  TdxSpreadSheetHyperlinkAssignedValue = (havDisplayText, havScreenTip, havValue, havExecuted);
  TdxSpreadSheetHyperlinkAssignedValues = set of TdxSpreadSheetHyperlinkAssignedValue;

  { TdxSpreadSheetHyperlink }

  TdxSpreadSheetHyperlink = class(TcxDoublyLinkedObject)
  strict private
    FData: TObject;
    FArea: TRect;
    FOwner: TdxSpreadSheetHyperlinks;
    function GetArea: TRect;
    function GetAssignedValues: TdxSpreadSheetHyperlinkAssignedValues;
    function GetDisplayText: string;
    function GetDisplayTextAssigned: Boolean;
    function GetIndex: Integer;
    function GetPrev: TdxSpreadSheetHyperlink;
    function GetMailAddress: string;
    function GetMailSubject: string;
    function GetNext: TdxSpreadSheetHyperlink;
    function GetReferenceArea: TRect;
    function GetReferenceName: TObject;
    function GetReferenceSheet: TObject;
    function GetScreenTip: string;
    function GetValue: string;
    function GetValueType: TdxSpreadSheetHyperlinkValueType;
    procedure SetAssignedValues(AValue: TdxSpreadSheetHyperlinkAssignedValues);
    procedure SetDisplayText(const AValue: string);
    procedure SetDisplayTextAssigned(AValue: Boolean);
    procedure SetIndex(AIndex: Integer);
    procedure SetMailAddress(AValue: string);
    procedure SetMailSubject(const AValue: string);
    procedure SetNext(AValue: TdxSpreadSheetHyperlink);
    procedure SetPrev(AValue: TdxSpreadSheetHyperlink);
    procedure SetScreenTip(const AValue: string);
    procedure SetValue(const AValue: string);
  protected
    procedure BeforeDestroy; virtual;
    procedure Changing; virtual;
    procedure Changed; virtual;
    function CreateData(AType: TdxSpreadSheetHyperlinkValueType): TObject; virtual;
    function GetContainer: TObject; virtual;
    function GetView: TObject; virtual;
    function IsHistoryAvailable: Boolean; virtual;
    function IsHyperlinkValid: Boolean;
    function IsReference(const AValue: string): Boolean;
    procedure ReplaceArea(const ANewArea: TRect);
    procedure ReplaceData(var AData: TObject; var AArea: TRect);
    procedure RelatedCellSetDisplayText; virtual;
    procedure RemoveContainerLink;
    procedure RemoveFromOwner; virtual;
    procedure RestoreStyle; virtual;
    procedure SetArea(const AArea: TRect);
    procedure SetDataValue(const AValue: string);
    procedure SetValueType(AValue: TdxSpreadSheetHyperlinkValueType);
    //
    procedure LoadFromStream(AReader: TcxReader); virtual;
    procedure SaveToStream(AWriter: TcxWriter); virtual;
    //
    property Data: TObject read FData write FData;
    property DisplayTextAssigned: Boolean read GetDisplayTextAssigned write SetDisplayTextAssigned;
    property Owner: TdxSpreadSheetHyperlinks read FOwner;
    property MailAddress: string read GetMailAddress write SetMailAddress;
    property MailSubject: string read GetMailSubject write SetMailSubject;
    property ReferenceArea: TRect read GetReferenceArea;
    property ReferenceName: TObject read GetReferenceName;
    property ReferenceSheet: TObject read GetReferenceSheet;
    property View: TObject read GetView;
  public
    constructor Create(AOwner: TdxSpreadSheetHyperlinks); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TdxSpreadSheetHyperlink); virtual;
    function Contains(const ARow, AColumn: Integer): Boolean;
    procedure Execute; virtual;
    function IsAreaCorrect: Boolean;

    property AssignedValues: TdxSpreadSheetHyperlinkAssignedValues read GetAssignedValues write SetAssignedValues;
    property Area: TRect read GetArea;
    property DisplayText: string read GetDisplayText write SetDisplayText;
    property Index: Integer read GetIndex write SetIndex;
    property Next: TdxSpreadSheetHyperlink read GetNext write SetNext;
    property Prev: TdxSpreadSheetHyperlink read GetPrev write SetPrev;
    property ScreenTip: string read GetScreenTip write SetScreenTip;
    property Value: string read GetValue write SetValue;
    property ValueType: TdxSpreadSheetHyperlinkValueType read GetValueType;
  end;

  { TdxSpreadSheetHyperlinks }

  TdxSpreadSheetHyperlinks = class(TcxDoublyLinkedObjectList)
  strict private
    FCachedItem: TdxSpreadSheetHyperlink;
    FView: TObject;

    function GetFirst: TdxSpreadSheetHyperlink;
    function GetItem(AIndex: Integer): TdxSpreadSheetHyperlink;
    function GetLast: TdxSpreadSheetHyperlink;
  protected
    procedure AssignChanges(ASource, ADest: TdxSpreadSheetHyperlink);
    procedure Changed; virtual;
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
    procedure DeleteCells(const AArea: TRect); virtual;
    function InternalAdd: TdxSpreadSheetHyperlink;
    //
    procedure BeginHistoryAction;
    procedure EndHistoryAction;
    //
    procedure LoadFromStream(AReader: TcxReader); virtual;
    procedure SaveToStream(AWriter: TcxWriter); virtual;
    //
    property CachedItem: TdxSpreadSheetHyperlink read FCachedItem write FCachedItem;
    property View: TObject read FView;
  public
    constructor Create(AView: TObject); virtual;
    destructor Destroy; override;
    function Add(const AArea: TRect): TdxSpreadSheetHyperlink; reintroduce; overload;
    function Add(const AArea: string): TdxSpreadSheetHyperlink; reintroduce; overload;
    function Add(const ARow, AColumn: Integer): TdxSpreadSheetHyperlink; reintroduce; overload;
    procedure Clear; override;
    function FindItem(const ACellReference: string): TdxSpreadSheetHyperlink; overload;
    function FindItem(const ARow, AColumn: Integer): TdxSpreadSheetHyperlink; overload;

    property First: TdxSpreadSheetHyperlink read GetFirst;
    property Last: TdxSpreadSheetHyperlink read GetLast;
    property Items[Index: Integer]: TdxSpreadSheetHyperlink read GetItem; default;
  end;

const
  dxEMailHyperlinkMailtoPrefix = 'mailto:';
  dxEMailHyperlinkSubjectPrefix = '?subject=';

implementation

uses
  Windows, Graphics, ShellApi, dxTypeHelpers, cxGeometry,  dxSpreadSheetCore, dxSpreadSheetStrs, dxSpreadSheetClasses,
  dxSpreadSheetTypes, dxSpreadSheetFormulas, dxSpreadSheetCoreStyles, dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasParser, dxSpreadSheetCoreFormulasTokens, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetObjectListAccess = class(TdxSpreadSheetObjectList);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetFontAccess = class(TdxSpreadSheetCustomFont);

  { TdxSpreadSheetHyperlinkData }

  TdxSpreadSheetHyperlinkData = class
  public
    AssignedValues: TdxSpreadSheetHyperlinkAssignedValues;
    DisplayText: string;
    ScreenTip: string;
    Value: string;
    procedure Assign(ASource: TdxSpreadSheetHyperlinkData); virtual;
    procedure CheckDisplayTextAssigned(AView: TdxSpreadSheetTableView); virtual;
    function GetDisplayText(AView: TdxSpreadSheetTableView): string; virtual;
    function GetValue(AView: TdxSpreadSheetTableView): string; virtual;
    procedure LoadFromStream(ASpreadSheet: TdxCustomSpreadSheet; AReader: TcxReader); virtual;
    procedure SaveToStream(ASpreadSheet: TdxCustomSpreadSheet; AWriter: TcxWriter); virtual;
    procedure SetValue(AView: TdxSpreadSheetTableView; const AValue: string); virtual;
    procedure Validate(ASpreadSheet: TdxCustomSpreadSheet); virtual;
  end;

  {TdxSpreadSheetHyperlinkDocumentReference }

  TdxSpreadSheetHyperlinkDocumentReference = class(TdxSpreadSheetHyperlinkData)
  public
    Area: TRect;
    Name: TdxSpreadSheetDefinedName;
    Sheet: TdxSpreadSheetCustomView;
    procedure Assign(ASource: TdxSpreadSheetHyperlinkData); override;
    procedure CheckDisplayTextAssigned(AView: TdxSpreadSheetTableView); override;
    function Correct: Boolean;
    procedure Execute(AView: TdxSpreadSheetTableView);
    function GetDisplayText(AView: TdxSpreadSheetTableView): string; override;
    function GetValue(AView: TdxSpreadSheetTableView): string; override;
    procedure LoadFromStream(ASpreadSheet: TdxCustomSpreadSheet; AReader: TcxReader); override;
    procedure SaveToStream(ASpreadSheet: TdxCustomSpreadSheet; AWriter: TcxWriter); override;
    procedure SetValue(AView: TdxSpreadSheetTableView; const AValue: string); override;
    procedure Validate(ASpreadSheet: TdxCustomSpreadSheet); override;
  end;

  { TdxSpreadSheetHyperlinkEMailReference }

  TdxSpreadSheetHyperlinkEMailReference = class(TdxSpreadSheetHyperlinkData)
  public
    Subject: string;
    procedure Assign(ASource: TdxSpreadSheetHyperlinkData); override;
    function GetValue(AView: TdxSpreadSheetTableView): string; override;
    procedure LoadFromStream(ASpreadSheet: TdxCustomSpreadSheet; AReader: TcxReader); override;
    procedure SaveToStream(ASpreadSheet: TdxCustomSpreadSheet; AWriter: TcxWriter); override;
    procedure SetValue(AView: TdxSpreadSheetTableView; const AValue: string); override;
  end;

  { TdxSpreadSheetHyperlinkReferenceHelper }

  TdxSpreadSheetHyperlinkReferenceHelper = class
  protected
    Formula: TdxSpreadSheetFormula;
    Parser: TdxSpreadSheetFormulaReferencesParser;
    Reference: TdxSpreadSheetHyperlinkDocumentReference;
    SpreadSheet: TdxCustomSpreadSheet;
    procedure ClearReference;
    function ConvertTokenToReference(AToken: TdxSpreadSheetFormulaToken): Boolean;
    function TryTokensToReference(AToken: TdxSpreadSheetFormulaToken): Boolean;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet; ADest: TdxSpreadSheetHyperlinkDocumentReference);
    procedure Initialize;
    destructor Destroy; override;
  end;

  TdxSpreadSheetHyperlinkReferenceHelperFormula = class(TdxSpreadSheetDefinedNameFormula)
  strict private
    FOwner: TdxSpreadSheetHyperlinkReferenceHelper;
  protected
    function GetController: TdxSpreadSheetCustomFormulaController; override;
  public
    constructor Create(AOwner: TdxSpreadSheetHyperlinkReferenceHelper); reintroduce; overload;
  end;

  { TdxSpreadSheetHistoryChangeHyperlinkAction }

  TdxSpreadSheetHistoryChangeHyperlinkAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetHistoryChangeHyperlinkCommand }

  TdxSpreadSheetHistoryChangeHyperlinkCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    function GetContainer: TdxSpreadSheetContainer;
    function GetHyperlinks: TdxSpreadSheetHyperlinks;
  protected
    Area: TRect;
    Data: TObject;
    ContainerIndex: Integer;
    Count, ItemIndex: Integer;
    procedure UndoRedo; virtual;
  public
    constructor Create(AHyperlink: TdxSpreadSheetHyperlink; ANewItem: Boolean = False);
    destructor Destroy; override;
    class function ActionClass: TdxSpreadSheetHistoryActionClass; override;
    procedure Redo; override;
    procedure Undo; override;

    property Container: TdxSpreadSheetContainer read GetContainer;
    property Hyperlinks: TdxSpreadSheetHyperlinks read GetHyperlinks;
  end;

const
  OptionsMap: array[Boolean] of TdxSpreadSheetCellReferenceOptions =
    ([], [croSheetName]);

{ TdxSpreadSheetHyperlinkData }

procedure TdxSpreadSheetHyperlinkData.Assign(ASource: TdxSpreadSheetHyperlinkData);
begin
  if ASource is TdxSpreadSheetHyperlinkData then
  begin
    AssignedValues := ASource.AssignedValues;
    DisplayText := ASource.DisplayText;
    ScreenTip := ASource.ScreenTip;
    Value := ASource.Value;
  end;
end;

procedure TdxSpreadSheetHyperlinkData.CheckDisplayTextAssigned(AView: TdxSpreadSheetTableView);
begin
end;

function TdxSpreadSheetHyperlinkData.GetDisplayText(AView: TdxSpreadSheetTableView): string;
begin
  if havDisplayText in AssignedValues then
    Result := DisplayText
  else
    Result := GetValue(AView);
end;

function TdxSpreadSheetHyperlinkData.GetValue(AView: TdxSpreadSheetTableView): string;
begin
  Result := Value;
end;

procedure TdxSpreadSheetHyperlinkData.LoadFromStream(ASpreadSheet: TdxCustomSpreadSheet; AReader: TcxReader);
begin
  DisplayText := AReader.ReadWideString;
  ScreenTip := AReader.ReadWideString;
  Value := AReader.ReadWideString;
  Byte(AssignedValues) := AReader.ReadByte;
end;

procedure TdxSpreadSheetHyperlinkData.SaveToStream(ASpreadSheet: TdxCustomSpreadSheet; AWriter: TcxWriter);
begin
  Validate(ASpreadSheet);
  AWriter.WriteWideString(DisplayText);
  AWriter.WriteWideString(ScreenTip);
  AWriter.WriteWideString(Value);
  AWriter.WriteByte(Byte(AssignedValues - [havExecuted]));
end;

procedure TdxSpreadSheetHyperlinkData.SetValue(AView: TdxSpreadSheetTableView; const AValue: string);
begin
  Value := AValue;
end;

procedure TdxSpreadSheetHyperlinkData.Validate(ASpreadSheet: TdxCustomSpreadSheet);
begin
end;

{TdxSpreadSheetHyperlinkDocumentReference }

procedure TdxSpreadSheetHyperlinkDocumentReference.Assign(ASource: TdxSpreadSheetHyperlinkData);
begin
  inherited Assign(ASource);
  if ASource is TdxSpreadSheetHyperlinkDocumentReference then
  begin
    Name := TdxSpreadSheetHyperlinkDocumentReference(ASource).Name;
    Sheet := TdxSpreadSheetHyperlinkDocumentReference(ASource).Sheet;
    Area := TdxSpreadSheetHyperlinkDocumentReference(ASource).Area;
  end;
end;

procedure TdxSpreadSheetHyperlinkDocumentReference.CheckDisplayTextAssigned(AView: TdxSpreadSheetTableView);

  function PrepareReferenceText(const AText: string): string;
  var
    I: Integer;
  const
    AChars: array[0..1] of string = ('''', '"');
  begin
    Result := AText;
    for I := 0 to High(AChars) do
      Result := StringReplace(Result, AChars[I], '', [rfReplaceAll]);
  end;

  function GetDefaultDisplayText(ARCReference: Boolean): string;
  var
    ASheet: TdxSpreadSheetTableView;
  begin
    ASheet := TdxSpreadSheetTableView(Sheet);
    if ASheet = nil then
      ASheet := AView;
    Result := PrepareReferenceText(dxReferenceToString(Area, ARCReference, OptionsMap[AView <> Sheet], ASheet.Caption));
  end;

var
  AText: string;
begin
  if (Name = nil) and Area.IsEqual(dxSpreadSheetGetRealArea(Area)) and ((Sheet <> nil) or (AView <> nil)) then
  begin
    AText := PrepareReferenceText(DisplayText);
    if (AText <> '') and (SameText(AText, GetDefaultDisplayText(False)) or SameText(AText, GetDefaultDisplayText(True))) then
      Exclude(AssignedValues, havDisplayText);
  end;
end;

function TdxSpreadSheetHyperlinkDocumentReference.Correct: Boolean;
begin
  Result := (Name <> nil) or (Sheet <> nil) and Area.IsEqual(dxSpreadSheetGetRealArea(Area));
end;

procedure TdxSpreadSheetHyperlinkDocumentReference.Execute(AView: TdxSpreadSheetTableView);
var
  R: TRect;
  ASpreadSheet: TdxCustomSpreadSheet;
begin
  ASpreadSheet := AView.SpreadSheet;
  Validate(ASpreadSheet);
  if not Correct then
    Exit;
  if Sheet <> nil then
    ASpreadSheet.ActiveSheet := Sheet
  else
    ASpreadSheet.ActiveSheet := AView;
  R := Area;
   if (R.Top <> R.Bottom) or (R.Left <> R.Right) then
     ASpreadSheet.ActiveSheetAsTable.Selection.Add(R)
   else
     ASpreadSheet.ActiveSheetAsTable.Selection.SetFocused(R.Top, R.Left, []);
end;

function TdxSpreadSheetHyperlinkDocumentReference.GetDisplayText(AView: TdxSpreadSheetTableView): string;
begin
  Result := inherited GetDisplayText(AView);
  if not (havDisplayText in AssignedValues) and Correct then
    Result := dxSpreadSheetFormulaExcludeEqualSymbol(Result);
end;

function TdxSpreadSheetHyperlinkDocumentReference.GetValue(AView: TdxSpreadSheetTableView): string;
var
  ASpreadSheet: TdxCustomSpreadSheet;
begin
  ASpreadSheet := AView.SpreadSheet;
  Validate(ASpreadSheet);
  if Name <> nil then
    Result := dxSpreadSheetFormulaIncludeEqualSymbol(Name.ToString)
  else
    if Correct then
    begin
      Result := dxDefaultOperations[opEQ] +
        dxReferenceToString(Area, ASpreadSheet.OptionsView.R1C1Reference,
        OptionsMap[AView <> Sheet], Sheet.Caption);
    end
    else
      Result := inherited GetValue(AView);
end;

procedure TdxSpreadSheetHyperlinkDocumentReference.LoadFromStream(
  ASpreadSheet: TdxCustomSpreadSheet; AReader: TcxReader);
var
  I: Integer;
  AValue: string;
begin
  inherited LoadFromStream(ASpreadSheet, AReader);
  Area := AReader.ReadRect;
  if AReader.ReadBoolean then
  begin
    AValue := AReader.ReadWideString;
    for I := 0 to ASpreadSheet.DefinedNames.Count - 1 do
      if ASpreadSheet.DefinedNames[I].ToString = AValue then
      begin
        Name := ASpreadSheet.DefinedNames[I];
        Break;
      end;
  end;
  if AReader.ReadBoolean then
    Sheet := ASpreadSheet.GetSheetByName(AReader.ReadWideString);
end;

procedure TdxSpreadSheetHyperlinkDocumentReference.SaveToStream(ASpreadSheet: TdxCustomSpreadSheet; AWriter: TcxWriter);
begin
  inherited SaveToStream(ASpreadSheet, AWriter);
  AWriter.WriteRect(Area);
  AWriter.WriteBoolean(Name <> nil);
  if Name <> nil then
    AWriter.WriteWideString(Name.ToString);
  AWriter.WriteBoolean(Sheet <> nil);
  if Sheet <> nil then
    AWriter.WriteWideString(Sheet.Caption);
end;

procedure TdxSpreadSheetHyperlinkDocumentReference.SetValue(AView: TdxSpreadSheetTableView; const AValue: string);
begin
  inherited SetValue(AView, dxSpreadSheetFormulaExcludeEqualSymbol(AValue));
  with TdxSpreadSheetHyperlinkReferenceHelper.Create(AView.SpreadSheet, Self) do
  try
    Initialize;
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetHyperlinkDocumentReference.Validate(ASpreadSheet: TdxCustomSpreadSheet);
begin
  if TdxSpreadSheetAccess(ASpreadSheet).FSheets.IndexOf(Sheet) < 0 then
    Sheet := nil;
  if TdxSpreadSheetObjectListAccess(ASpreadSheet.DefinedNames).ItemList.IndexOf(Name) < 0 then
    Name := nil;
end;

{ TdxSpreadSheetHyperlinkEMailReference }

procedure TdxSpreadSheetHyperlinkEMailReference.Assign(ASource: TdxSpreadSheetHyperlinkData);
begin
  inherited Assign(ASource);
  if ASource is TdxSpreadSheetHyperlinkEMailReference then
    Subject := TdxSpreadSheetHyperlinkEMailReference(ASource).Subject;
end;

function TdxSpreadSheetHyperlinkEMailReference.GetValue(AView: TdxSpreadSheetTableView): string;
begin
  Result := dxEMailHyperlinkMailtoPrefix + inherited GetValue(AView);
  if Trim(Subject) <> '' then
    Result := Result + dxEMailHyperlinkSubjectPrefix + Trim(Subject);
end;

procedure TdxSpreadSheetHyperlinkEMailReference.LoadFromStream(ASpreadSheet: TdxCustomSpreadSheet; AReader: TcxReader);
begin
  inherited LoadFromStream(ASpreadSheet, AReader);
  Subject := AReader.ReadWideString;
end;

procedure TdxSpreadSheetHyperlinkEMailReference.SaveToStream(ASpreadSheet: TdxCustomSpreadSheet; AWriter: TcxWriter);
begin
  inherited SaveToStream(ASpreadSheet, AWriter);
  AWriter.WriteWideString(Subject);
end;

procedure TdxSpreadSheetHyperlinkEMailReference.SetValue(AView: TdxSpreadSheetTableView; const AValue: string);
var
  AAddressPosition, ASubjectPosition: Integer;
begin
  Value := Trim(AValue);
  AAddressPosition := Pos(dxEMailHyperlinkMailtoPrefix, LowerCase(Value));
  ASubjectPosition := Pos(dxEMailHyperlinkSubjectPrefix, LowerCase(Value));
  Subject := '';
  if ASubjectPosition = 0 then
    ASubjectPosition := Length(AValue) + 1
  else
  begin
    Subject := Copy(Value, ASubjectPosition + Length(dxEMailHyperlinkSubjectPrefix), Length(AValue));
    Delete(Value, ASubjectPosition, ASubjectPosition + Length(AValue));
  end;
  if AAddressPosition <> 0 then
  begin
    Inc(AAddressPosition, Length(dxEMailHyperlinkMailtoPrefix));
    Value := Copy(AValue, AAddressPosition, ASubjectPosition - AAddressPosition);
  end
end;

{ TdxSpreadSheetHyperlinkReferenceHelper }

constructor TdxSpreadSheetHyperlinkReferenceHelper.Create(
  ASpreadSheet: TdxCustomSpreadSheet; ADest: TdxSpreadSheetHyperlinkDocumentReference);
begin
  SpreadSheet := ASpreadSheet;
  Reference := ADest;
  Formula := TdxSpreadSheetHyperlinkReferenceHelperFormula.Create(Self);
  Parser := TdxSpreadSheetFormulaReferencesParser.Create(ASpreadSheet);
  Parser.ParseFormula(dxSpreadSheetFormulaIncludeEqualSymbol(ADest.Value), Formula);
end;

destructor TdxSpreadSheetHyperlinkReferenceHelper.Destroy;
begin
  FreeAndNil(Parser);
  FreeAndNil(Formula);
  inherited Destroy;
end;

procedure TdxSpreadSheetHyperlinkReferenceHelper.Initialize;
begin
  if (Formula = nil) or not TryTokensToReference(Formula.Tokens) then
    ClearReference;
end;

procedure TdxSpreadSheetHyperlinkReferenceHelper.ClearReference;
begin
  Reference.Sheet := nil;
  Reference.Name := nil;
  Reference.Area := cxInvalidRect;
end;

function TdxSpreadSheetHyperlinkReferenceHelper.ConvertTokenToReference(AToken: TdxSpreadSheetFormulaToken): Boolean;
begin
  ClearReference;
  if AToken is TdxSpreadSheetFormulaReference then
  begin
    Reference.Sheet := TdxSpreadSheetFormulaReference(AToken).View as TdxSpreadSheetCustomView;
    Reference.Area.Left := TdxSpreadSheetFormulaReference(AToken).ActualColumn;
    Reference.Area.Top := TdxSpreadSheetFormulaReference(AToken).ActualRow;
    if AToken is TdxSpreadSheetFormulaAreaReference then
    begin
      Reference.Area.Bottom := TdxSpreadSheetFormulaAreaReference(AToken).ActualRow2;
      Reference.Area.Right := TdxSpreadSheetFormulaAreaReference(AToken).ActualColumn2;
    end
    else
      Reference.Area.BottomRight := Reference.Area.TopLeft;
  end
  else
    if AToken is TdxSpreadSheetDefinedNameToken then
    begin
      Reference.Name := TdxSpreadSheetDefinedNameToken(AToken).DefinedName;
      Reference.Area := cxNullRect;
    end;
  Result := Reference.Correct;
  if not Result then
    ClearReference
  else
    Reference.CheckDisplayTextAssigned(nil)

end;

function TdxSpreadSheetHyperlinkReferenceHelper.TryTokensToReference(AToken: TdxSpreadSheetFormulaToken): Boolean;
begin
  if (AToken is TdxSpreadSheetFormulaReference) or (AToken is TdxSpreadSheetDefinedNameToken) then
    Result := ConvertTokenToReference(AToken)
  else
  begin
    Result := ((AToken <> nil) and  (AToken.ChildCount = 1)) and
      (AToken.Next = nil) and (AToken.Prev = nil);
    if Result then
      AToken := AToken.FirstChild;
    Result := Result and (AToken is TdxSpreadSheetFormulaReference) or (AToken is TdxSpreadSheetDefinedNameToken);
    if Result then
      Result := ConvertTokenToReference(AToken)
  end;
end;

{ TdxSpreadSheetHyperlinkReferenceHelperFormula }

constructor TdxSpreadSheetHyperlinkReferenceHelperFormula.Create(AOwner: TdxSpreadSheetHyperlinkReferenceHelper);
begin
  FOwner := AOwner;
end;

function TdxSpreadSheetHyperlinkReferenceHelperFormula.GetController: TdxSpreadSheetCustomFormulaController;
begin
  Result := FOwner.SpreadSheet.FormulaController;
end;

{ TdxSpreadSheetHyperlink }

constructor TdxSpreadSheetHyperlink.Create(AOwner: TdxSpreadSheetHyperlinks);
begin
  FOwner := AOwner;
  FData := CreateData(hvtFileOrWebPage);
end;

destructor TdxSpreadSheetHyperlink.Destroy;
begin
  if IsHistoryAvailable then
  begin
    Owner.BeginHistoryAction;
    try
      TdxSpreadSheetTableView(View).History.AddCommand(TdxSpreadSheetHistoryChangeHyperlinkCommand.Create(Self));
      BeforeDestroy;
    finally
      Owner.EndHistoryAction;
     end;
  end
  else
    BeforeDestroy;
  inherited Destroy;
end;

procedure TdxSpreadSheetHyperlink.Assign(ASource: TdxSpreadSheetHyperlink);
begin
  if ASource is TdxSpreadSheetHyperlink then
  begin
    Changing;
    try
      FArea := ASource.Area;
      SetValueType(ASource.ValueType);
      TdxSpreadSheetHyperlinkData(Data).Assign(TdxSpreadSheetHyperlinkData(ASource.Data));
      RelatedCellSetDisplayText;
    finally
      Changed;
    end;
  end;
end;

function TdxSpreadSheetHyperlink.Contains(const ARow, AColumn: Integer): Boolean;
begin
  Result := dxSpreadSheetContains(Area, ARow, AColumn);
end;

procedure TdxSpreadSheetHyperlink.Execute;
var
  AView: TdxSpreadSheetTableView;
begin
  if Value = '' then
    Exit;

  AView := TdxSpreadSheetTableView(Owner.View);
  AView.Controller.CellHintController.Hide;
  if not TdxSpreadSheetAccess(AView.SpreadSheet).DoHyperlinkExecute(Self) then
  begin
    if ValueType = hvtReference then
      TdxSpreadSheetHyperlinkDocumentReference(Data).Execute(AView)
    else
      if not dxShellExecute(AView.SpreadSheet.Handle, Value) then
        raise EdxSpreadSheetHyperlinkExecuteError.CreateFmt(cxGetResourceString(@sdxHyperlinkExecuteError), [Value]);

    Include(TdxSpreadSheetHyperlinkData(Data).AssignedValues, havExecuted);
  end;
end;

procedure TdxSpreadSheetHyperlink.BeforeDestroy;
begin
  RestoreStyle;
  RemoveContainerLink;
  RemoveFromOwner;
  FreeAndNil(FData);
end;

procedure TdxSpreadSheetHyperlink.Changing;
begin
  // do nothing
end;

procedure TdxSpreadSheetHyperlink.Changed;
begin
  Owner.Changed;
end;

function TdxSpreadSheetHyperlink.CreateData(AType: TdxSpreadSheetHyperlinkValueType): TObject;
begin
  case AType of
    hvtReference:
      Result := TdxSpreadSheetHyperlinkDocumentReference.Create;
    hvtEMail:
      Result := TdxSpreadSheetHyperlinkEMailReference.Create;
  else
    Result := TdxSpreadSheetHyperlinkData.Create;
  end;
  if Data <> nil then
    TdxSpreadSheetHyperlinkData(Result).Assign(TdxSpreadSheetHyperlinkData(Data));
  TdxSpreadSheetHyperlinkData(Result).CheckDisplayTextAssigned(TdxSpreadSheetTableView(View));
end;

function TdxSpreadSheetHyperlink.GetContainer: TObject;
var
  I: Integer;
  AView: TdxSpreadSheetTableView;
begin
  AView := TdxSpreadSheetTableView(Owner.View);
  Result := nil;
  if AView <> nil then
  begin
    for I := 0 to AView.Containers.Count - 1 do
      if AView.Containers[I].Hyperlink = Self then
      begin
        Result := AView.Containers[I];
        Break;
      end;
  end;
end;

function TdxSpreadSheetHyperlink.GetView: TObject;
begin
  Result := Owner.View;
end;

function TdxSpreadSheetHyperlink.IsAreaCorrect: Boolean;
begin
  Result := dxSpreadSheetIsValidArea(FArea);
end;

function TdxSpreadSheetHyperlink.IsHistoryAvailable: Boolean;
begin
  Result := (Owner <> nil) and (View <> nil) and TdxSpreadSheetTableView(View).History.CanAddCommand;
end;

function TdxSpreadSheetHyperlink.IsHyperlinkValid: Boolean;
begin
  Result := Value <> '';
  if Result and (ValueType = hvtReference) then
  begin
    TdxSpreadSheetHyperlinkDocumentReference(Data).Validate(TdxSpreadSheetTableView(View).SpreadSheet);
    Result := TdxSpreadSheetHyperlinkDocumentReference(Data).Correct;
  end;
end;

function TdxSpreadSheetHyperlink.IsReference(const AValue: string): Boolean;
begin
  Result := dxSpreadSheetIsFormula(AValue);
end;

procedure TdxSpreadSheetHyperlink.ReplaceArea(const ANewArea: TRect);
begin
  if IsHistoryAvailable then
    TdxSpreadSheetTableView(View).History.AddCommand(TdxSpreadSheetHistoryChangeHyperlinkCommand.Create(Self));
  FArea := ANewArea;
end;

procedure TdxSpreadSheetHyperlink.ReplaceData(var AData: TObject; var AArea: TRect);
var
  R: TRect;
begin
  ExchangePointers(FData, AData);
  R := FArea;
  FArea := AArea;
  AArea := R;
end;

procedure TdxSpreadSheetHyperlink.RelatedCellSetDisplayText;
var
  ARow, AColumn: Integer;
  ACell: TdxSpreadSheetCell;
  AFont: TdxSpreadSheetFontAccess;
begin
  if not IsAreaCorrect or (TdxSpreadSheetAccess(TdxSpreadSheetTableView(View).SpreadSheet).State <> []) then
    Exit;
  TdxSpreadSheetTableView(View).BeginUpdate;
  try
    ACell := TdxSpreadSheetTableView(View).Cells[Area.Top, Area.Left];
    if ((ACell = nil) or (ACell.AsString = '')) and (DisplayText <> '') or DisplayTextAssigned then
      TdxSpreadSheetTableView(View).CreateCell(Area.Top, Area.Left).SetText(DisplayText);
    for ARow := Area.Top to Area.Bottom do
      for AColumn := Area.Left to Area.Right do
      begin
        AFont := TdxSpreadSheetFontAccess(TdxSpreadSheetTableView(View).CreateCell(ARow, AColumn).Style.Font);
        AFont.ChangeFont(AFont.Name, AFont.Charset,
          TdxSpreadSheetAccess(TdxSpreadSheetTableView(View).SpreadSheet).LookAndFeelPainter.DefaultHyperlinkTextColor,
          AFont.Size, AFont.Pitch, AFont.Style + [fsUnderline]);
      end;
  finally
    TdxSpreadSheetTableView(View).EndUpdate;
  end;
end;

procedure TdxSpreadSheetHyperlink.RemoveContainerLink;
begin
  if GetContainer <> nil then
    TdxSpreadSheetContainerAccess(GetContainer).ReleaseHyperlink;
end;

procedure TdxSpreadSheetHyperlink.RemoveFromOwner;
begin
  Owner.CachedItem := nil;
  Owner.Extract(Self);
end;

procedure TdxSpreadSheetHyperlink.RestoreStyle;
var
  ARow, AColumn: Integer;
begin
  if Owner.View = nil then
    Exit;
  TdxSpreadSheetTableView(View).BeginUpdate;
  try
    for ARow := Area.Top to Area.Bottom do
      for AColumn := Area.Left to Area.Right do
        TdxSpreadSheetTableView(View).CreateCell(ARow, AColumn).Style.Font.Assign(
          TdxSpreadSheetTableView(View).SpreadSheet.DefaultCellStyle.Font);
  finally
    TdxSpreadSheetTableView(View).EndUpdate;
  end;
end;

procedure TdxSpreadSheetHyperlink.SetArea(const AArea: TRect);
begin
  Changing;
  try
    FArea := AArea;
    RelatedCellSetDisplayText;
  finally
    Changed;
  end;
end;

procedure TdxSpreadSheetHyperlink.SetDataValue(const AValue: string);
begin
  TdxSpreadSheetHyperlinkData(Data).SetValue(TdxSpreadSheetTableView(Owner.View), AValue);
  Include(TdxSpreadSheetHyperlinkData(Data).AssignedValues, havValue);
end;

procedure TdxSpreadSheetHyperlink.SetValueType(AValue: TdxSpreadSheetHyperlinkValueType);
var
  AData: TObject;
begin
  if AValue <> ValueType then
  begin
    AData := FData;
    FData := CreateData(AValue);
    AData.Free;
  end;
end;

procedure TdxSpreadSheetHyperlink.LoadFromStream(AReader: TcxReader);
begin
  FArea := AReader.ReadRect;
  SetValueType(TdxSpreadSheetHyperlinkValueType(AReader.ReadByte));
  TdxSpreadSheetHyperlinkData(Data).LoadFromStream(TdxSpreadSheetTableView(Owner.View).SpreadSheet, AReader);
end;

procedure TdxSpreadSheetHyperlink.SaveToStream(AWriter: TcxWriter);
begin
  AWriter.WriteRect(Area);
  AWriter.WriteByte(Byte(ValueType));
  TdxSpreadSheetHyperlinkData(Data).SaveToStream(TdxSpreadSheetTableView(Owner.View).SpreadSheet, AWriter);
end;

procedure TdxSpreadSheetHyperlink.SetValue(const AValue: string);
begin
  if SameText(AValue, Value) then
    Exit;
  Changing;
  try
    if Pos(dxEMailHyperlinkMailtoPrefix, LowerCase(AValue)) > 0 then
      SetValueType(hvtEMail)
    else
      if IsReference(AValue) then
        SetValueType(hvtReference)
      else
        SetValueType(hvtFileOrWebPage);
    SetDataValue(AValue);
    RelatedCellSetDisplayText;
  finally
    Changed;
  end;
end;

procedure TdxSpreadSheetHyperlink.SetAssignedValues(AValue: TdxSpreadSheetHyperlinkAssignedValues);
begin
  if AssignedValues <> AValue then
  begin
    Changing;
    try
      TdxSpreadSheetHyperlinkData(Data).AssignedValues := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetHyperlink.SetDisplayText(const AValue: string);
begin
  if (DisplayText = AValue) and DisplayTextAssigned then
    Exit;
  Changing;
  try
    TdxSpreadSheetHyperlinkData(Data).DisplayText := AValue;
    DisplayTextAssigned := havDisplayText in TdxSpreadSheetHyperlinkData(Data).AssignedValues;
    RelatedCellSetDisplayText;
  finally
    Changed;
  end;
end;

procedure TdxSpreadSheetHyperlink.SetDisplayTextAssigned(AValue: Boolean);
begin
  Include(TdxSpreadSheetHyperlinkData(Data).AssignedValues, havDisplayText);
end;

procedure TdxSpreadSheetHyperlink.SetScreenTip(const AValue: string);
begin
  if (ScreenTip = AValue) and (havScreenTip in AssignedValues) then
    Exit;
  Changing;
  try
    TdxSpreadSheetHyperlinkData(Data).ScreenTip := AValue;
    Include(TdxSpreadSheetHyperlinkData(Data).AssignedValues, havScreenTip);
  finally
    Changed;
  end;
end;

function TdxSpreadSheetHyperlink.GetArea: TRect;
begin
  Result := FArea;
  Result.Left := Max(0, Result.Left);
  Result.Top := Max(0, Result.Top);
end;

function TdxSpreadSheetHyperlink.GetAssignedValues: TdxSpreadSheetHyperlinkAssignedValues;
begin
  Result := TdxSpreadSheetHyperlinkData(Data).AssignedValues;
end;

function TdxSpreadSheetHyperlink.GetDisplayText: string;
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := TdxSpreadSheetTableView(Owner.View).Cells[Area.Top, Area.Left];
  if not DisplayTextAssigned and (ACell <> nil) and not ACell.IsEmpty then
    Result := ACell.AsString
  else
    Result := TdxSpreadSheetHyperlinkData(Data).GetDisplayText(TdxSpreadSheetTableView(Owner.View));
end;

function TdxSpreadSheetHyperlink.GetDisplayTextAssigned: Boolean;
begin
  Result := havDisplayText in AssignedValues;
end;

function TdxSpreadSheetHyperlink.GetIndex: Integer;
var
  AItem: TcxDoublyLinkedObject;
begin
  Result := 0;
  AItem := Prev;
  while AItem <> nil do
  begin
    Inc(Result);
    AItem := AItem.Prev;
  end;
end;

function TdxSpreadSheetHyperlink.GetPrev: TdxSpreadSheetHyperlink;
begin
  Result := TdxSpreadSheetHyperlink(inherited Prev);
end;

function TdxSpreadSheetHyperlink.GetMailAddress: string;
begin
  Result := TdxSpreadSheetHyperlinkData(Data).Value;
end;

function TdxSpreadSheetHyperlink.GetMailSubject: string;
begin
  if Data is TdxSpreadSheetHyperlinkEMailReference then
    Result := TdxSpreadSheetHyperlinkEMailReference(Data).Subject
  else
    Result := '';
end;

function TdxSpreadSheetHyperlink.GetNext: TdxSpreadSheetHyperlink;
begin
  Result := TdxSpreadSheetHyperlink(inherited Next);
end;

function TdxSpreadSheetHyperlink.GetReferenceArea: TRect;
begin
  Result := cxInvalidRect;
  if Data is TdxSpreadSheetHyperlinkDocumentReference then
  begin
    TdxSpreadSheetHyperlinkDocumentReference(Data).Validate(TdxSpreadSheetTableView(View).SpreadSheet);
    Result := TdxSpreadSheetHyperlinkDocumentReference(Data).Area;
  end;
end;

function TdxSpreadSheetHyperlink.GetReferenceName: TObject;
begin
  Result := nil;
  if Data is TdxSpreadSheetHyperlinkDocumentReference then
  begin
    TdxSpreadSheetHyperlinkDocumentReference(Data).Validate(TdxSpreadSheetTableView(View).SpreadSheet);
    Result := TdxSpreadSheetHyperlinkDocumentReference(Data).Name;
  end;
end;

function TdxSpreadSheetHyperlink.GetReferenceSheet: TObject;
begin
  Result := nil;
  if Data is TdxSpreadSheetHyperlinkDocumentReference then
  begin
    TdxSpreadSheetHyperlinkDocumentReference(Data).Validate(TdxSpreadSheetTableView(View).SpreadSheet);
    Result := TdxSpreadSheetHyperlinkDocumentReference(Data).Sheet;
  end;
end;

function TdxSpreadSheetHyperlink.GetScreenTip: string;
begin
  if havScreenTip in AssignedValues then
    Result := TdxSpreadSheetHyperlinkData(Data).ScreenTip
  else
    Result := '';
end;

function TdxSpreadSheetHyperlink.GetValue: string;
begin
  Result := TdxSpreadSheetHyperlinkData(Data).GetValue(TdxSpreadSheetTableView(Owner.View));
end;

function TdxSpreadSheetHyperlink.GetValueType: TdxSpreadSheetHyperlinkValueType;
begin
  if Data is TdxSpreadSheetHyperlinkDocumentReference then
    Result := hvtReference
  else
    if Data is TdxSpreadSheetHyperlinkEMailReference then
      Result := hvtEMail
    else
      Result := hvtFileOrWebPage;
end;

procedure TdxSpreadSheetHyperlink.SetIndex(AIndex: Integer);
begin
  AIndex := Min(AIndex, Owner.Count - 1);
  if (Owner.First = Owner.Last) or (AIndex = Index) then
    Exit;

  Owner.Extract(Self);

  // # find place
  Next := Owner.First;
  while (AIndex <> 0) and (Next <> nil) do
  begin
    Dec(AIndex);
    Next := Next.Next;
  end;

  // # insert into list
  Prev := nil;
  if Next <> nil then
  begin
    Prev := Next.Prev;
    Next.Prev := Self;
    if Prev <> nil then
      Prev.Next := Self;
  end;
end;

procedure TdxSpreadSheetHyperlink.SetMailAddress(AValue: string);
begin
  case ValueType of
    hvtReference:
      AValue := dxSpreadSheetFormulaIncludeEqualSymbol(AValue);
    hvtEMail:
    begin
      if Pos(dxEMailHyperlinkMailtoPrefix, LowerCase(AValue)) = 0 then
        AValue := dxEMailHyperlinkMailtoPrefix + AValue;
      AValue := AValue + dxEMailHyperlinkSubjectPrefix + MailSubject;
    end;
  end;
  TdxSpreadSheetHyperlinkData(Data).SetValue(TdxSpreadSheetTableView(Owner.View), AValue);
end;

procedure TdxSpreadSheetHyperlink.SetMailSubject(const AValue: string);
begin
  if Data is TdxSpreadSheetHyperlinkEMailReference then
    TdxSpreadSheetHyperlinkEMailReference(Data).Subject := AValue;
  Changed;
end;

procedure TdxSpreadSheetHyperlink.SetNext(AValue: TdxSpreadSheetHyperlink);
begin
  inherited Next := AValue;
end;

procedure TdxSpreadSheetHyperlink.SetPrev(AValue: TdxSpreadSheetHyperlink);
begin
  inherited Prev := AValue;
end;


{ TdxSpreadSheetHyperlinks }

constructor TdxSpreadSheetHyperlinks.Create(AView: TObject);
begin
  FView := AView;
end;

destructor TdxSpreadSheetHyperlinks.Destroy;
begin
  FView := nil;
  inherited Destroy;
end;

function TdxSpreadSheetHyperlinks.Add(const AArea: TRect): TdxSpreadSheetHyperlink;
var
  AHistory: TdxSpreadSheetHistory;
begin
  TdxSpreadSheetTableView(View).BeginUpdate;
  try
    Result := InternalAdd;
    AHistory := TdxSpreadSheetTableView(View).History;
    Result.SetArea(AArea);
    if AHistory.CanAddCommand then
    begin
      BeginHistoryAction;
      try
        AHistory.AddCommand(TdxSpreadSheetHistoryChangeHyperlinkCommand.Create(Result, True));
      finally
        EndHistoryAction;
      end;
    end;
    Changed;
  finally
    TdxSpreadSheetTableView(View).EndUpdate;
  end;
end;

function TdxSpreadSheetHyperlinks.Add(const AArea: string): TdxSpreadSheetHyperlink;
var
  R: TRect;
begin
  R := dxStringToReferenceArea(AArea);
  if not dxSpreadSheetIsValidArea(R) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidCellsReference), [AArea]);
  Result := Add(R);
end;

function TdxSpreadSheetHyperlinks.Add(const ARow, AColumn: Integer): TdxSpreadSheetHyperlink;
begin
  Result := Add(Rect(AColumn, ARow, AColumn, ARow));
end;

procedure TdxSpreadSheetHyperlinks.Clear;
begin
  while Last <> nil do
    Last.Free;
end;

function TdxSpreadSheetHyperlinks.FindItem(const ACellReference: string): TdxSpreadSheetHyperlink;
var
  ARow, AColumn: Integer;
begin
  dxStringToReference(ACellReference, AColumn, ARow);
  if not dxSpreadSheetIsValidCellReference(ARow, AColumn) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidCellReference), [ACellReference]);
  Result := FindItem(ARow, AColumn);
end;

function TdxSpreadSheetHyperlinks.FindItem(const ARow, AColumn: Integer): TdxSpreadSheetHyperlink;
begin
  Result := CachedItem;
  if (Result <> nil) and Result.Contains(ARow, AColumn) then
    Exit;
  Result := Last;
  while Result <> nil do
  begin
    if Result.Contains(ARow, AColumn) then
    begin
      CachedItem := Result;
      Exit;
    end;
    Result := TdxSpreadSheetHyperlink(Result.Prev);
  end;
  Result := nil;
end;

procedure TdxSpreadSheetHyperlinks.AssignChanges(ASource, ADest: TdxSpreadSheetHyperlink);
var
  ANewlyCreated: Boolean;
  AHistory: TdxSpreadSheetHistory;
begin
  TdxSpreadSheetTableView(View).BeginUpdate;
  try
    ANewlyCreated := ADest = nil;
    AHistory := TdxSpreadSheetTableView(View).History;
    if ADest = nil then
    begin
      ADest := InternalAdd;
      if TdxSpreadSheetTableView(View).Controller.FocusedContainer <> nil then
        TdxSpreadSheetTableView(View).Controller.FocusedContainer.Hyperlink := ADest;
    end;
    if AHistory.CanAddCommand then
    begin
      BeginHistoryAction;
      try
        AHistory.AddCommand(TdxSpreadSheetHistoryChangeHyperlinkCommand.Create(ADest, ANewlyCreated));
        ADest.Assign(ASource);
        if ASource.ScreenTip = '' then
          ADest.AssignedValues := ADest.AssignedValues - [havScreenTip];
      finally
        EndHistoryAction;
      end;
    end
    else
      ADest.Assign(ASource);
  finally
    TdxSpreadSheetTableView(View).EndUpdate;
  end;
end;

procedure TdxSpreadSheetHyperlinks.Changed;
begin
  CachedItem := nil;
  TdxSpreadSheetTableViewAccess(View).AddChanges([sscData, sscModified]);
end;

function TdxSpreadSheetHyperlinks.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := TdxSpreadSheetHyperlink.Create(Self);
end;

procedure TdxSpreadSheetHyperlinks.DeleteCells(const AArea: TRect);
var
  AItemToDelete, AItem: TdxSpreadSheetHyperlink;
begin
  AItem := First;
  while AItem <> nil do
  begin
    AItemToDelete := nil;
    if dxSpreadSheetContains(AArea, AItem.Area.Top, AItem.Area.Left) then
      AItemToDelete := AItem;
    AItem := AItem.Next;
    if AItemToDelete <> nil then
      AItemToDelete.Free;
  end;
end;

function TdxSpreadSheetHyperlinks.InternalAdd: TdxSpreadSheetHyperlink;
begin
  Result := TdxSpreadSheetHyperlink(inherited Add);
end;

procedure TdxSpreadSheetHyperlinks.BeginHistoryAction;
begin
  TdxSpreadSheetTableView(View).History.BeginAction(TdxSpreadSheetHistoryChangeHyperlinkAction);
end;

procedure TdxSpreadSheetHyperlinks.EndHistoryAction;
begin
  TdxSpreadSheetTableView(View).History.EndAction;
end;

procedure TdxSpreadSheetHyperlinks.LoadFromStream(AReader: TcxReader);
var
  I: Integer;
begin
  Clear;
  for I := 0 to AReader.ReadInteger - 1 do
    InternalAdd.LoadFromStream(AReader);
end;

procedure TdxSpreadSheetHyperlinks.SaveToStream(AWriter: TcxWriter);
var
  AItem: TdxSpreadSheetHyperlink;
begin
  AWriter.WriteInteger(Count);
  AItem := First;
  while AItem <> nil do
  begin
    AItem.SaveToStream(AWriter);
    AItem := TdxSpreadSheetHyperlink(AItem.Next);
  end;
end;

function TdxSpreadSheetHyperlinks.GetFirst: TdxSpreadSheetHyperlink;
begin
  Result := inherited First as TdxSpreadSheetHyperlink;
end;

function TdxSpreadSheetHyperlinks.GetItem(AIndex: Integer): TdxSpreadSheetHyperlink;
begin
  Result := First;
  while (Result <> nil) and (AIndex > 0) do
  begin
    Result := TdxSpreadSheetHyperlink(Result.Next);
    Dec(AIndex)
  end;
end;

function TdxSpreadSheetHyperlinks.GetLast: TdxSpreadSheetHyperlink;
begin
  Result := inherited Last as TdxSpreadSheetHyperlink;
end;

{ TdxSpreadSheetHistoryChangeHyperlinkAction }

class function TdxSpreadSheetHistoryChangeHyperlinkAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionChangeHyperlink);
end;

{ TdxSpreadSheetHistoryChangeHyperlinkCommand }

constructor TdxSpreadSheetHistoryChangeHyperlinkCommand.Create(AHyperlink: TdxSpreadSheetHyperlink; ANewItem: Boolean);
begin
  if AHyperlink.GetContainer <> nil then
    ContainerIndex := TdxSpreadSheetContainer(AHyperlink.GetContainer).Index
  else
    ContainerIndex := -1;
  Data := TdxSpreadSheetHyperlinkData(AHyperlink.CreateData(AHyperlink.ValueType));
  TdxSpreadSheetHyperlinkData(Data).Assign(TdxSpreadSheetHyperlinkData(AHyperlink.Data));
  Area := AHyperlink.Area;
  Count := AHyperlink.Owner.Count;
  if ANewItem then
    ItemIndex := Count
  else
    ItemIndex := TdxSpreadSheetHyperlink(AHyperlink).Index;
end;

destructor TdxSpreadSheetHistoryChangeHyperlinkCommand.Destroy;
begin
  FreeAndNil(Data);
  inherited Destroy;
end;

class function TdxSpreadSheetHistoryChangeHyperlinkCommand.ActionClass: TdxSpreadSheetHistoryActionClass;
begin
  Result := TdxSpreadSheetHistoryChangeHyperlinkAction;
end;

procedure TdxSpreadSheetHistoryChangeHyperlinkCommand.Redo;
begin
  UndoRedo;
end;

procedure TdxSpreadSheetHistoryChangeHyperlinkCommand.Undo;
begin
  UndoRedo;
end;

procedure TdxSpreadSheetHistoryChangeHyperlinkCommand.UndoRedo;
begin
  if Count > Hyperlinks.Count then
  begin
    Hyperlinks.InternalAdd.ReplaceData(Data, Area);
    if ItemIndex < Count then
      TdxSpreadSheetHyperlink(Hyperlinks.Last).Index := ItemIndex;
    if ContainerIndex >= 0 then
      Container.Hyperlink := TdxSpreadSheetHyperlink(Hyperlinks.Last);
  end
  else
    if (ItemIndex = Count) or (ContainerIndex >= 0) then
    begin
      Hyperlinks.Last.ReplaceData(Data, Area);
      Hyperlinks.Last.Free;
    end
    else
      Hyperlinks.Items[ItemIndex].ReplaceData(Data, Area);
end;

function TdxSpreadSheetHistoryChangeHyperlinkCommand.GetContainer: TdxSpreadSheetContainer;
begin
  Result := nil;
  if ContainerIndex >= 0 then
    Result := View.Containers[ContainerIndex];
end;

function TdxSpreadSheetHistoryChangeHyperlinkCommand.GetHyperlinks: TdxSpreadSheetHyperlinks;
begin
  Result := TdxSpreadSheetTableView(View).Hyperlinks;
end;

end.
