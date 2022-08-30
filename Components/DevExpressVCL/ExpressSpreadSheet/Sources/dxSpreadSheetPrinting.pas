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

unit dxSpreadSheetPrinting;

{$I cxVer.Inc}

interface

uses
  Windows, Types, Classes, Graphics, cxGeometry, dxCore, Generics.Defaults, Generics.Collections;

type

  { TdxSpreadSheetTableViewOptionsPrintCustomPersistent }

  TdxSpreadSheetTableViewOptionsPrintCustomPersistent = class(TPersistent)
  strict private
    FChangeLockCount: Integer;
    FOnChange: TNotifyEvent;
  protected
    procedure Changed; virtual;
    procedure ChangeHandler(Sender: TObject);
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AChangeHandler: TNotifyEvent = nil); virtual;
    procedure AfterConstruction; override;
    procedure Reset; virtual; abstract;
  end;

  { TdxSpreadSheetTableViewOptionsPrintHeaderFooterText }

  TdxSpreadSheetTableViewOptionsPrintHeaderFooterText = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  strict private const
    SectionCount = 3;
  strict private
    FAssigned: Boolean;
    FValues: array [0..SectionCount - 1] of string;

    function GetValue(const Index: Integer): string;
    procedure SetAssigned(const Value: Boolean);
    procedure SetValue(const Index: Integer; const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Assigned: Boolean read FAssigned write SetAssigned default False;
    property LeftSection: string index 0 read GetValue write SetValue;
    property CenterSection: string index 1 read GetValue write SetValue;
    property RightSection: string index 2 read GetValue write SetValue;
  end;

  { TdxSpreadSheetTableViewOptionsPrintHeaderFooter }

  TdxSpreadSheetTableViewOptionsPrintHeaderFooter = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  strict private
    FAlignWithMargins: TdxDefaultBoolean;
    FCommonFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FCommonHeader: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FEvenPagesFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FEvenPagesHeader: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FFirstPageFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FFirstPageHeader: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FScaleWithDocument: TdxDefaultBoolean;

    function GetActualAlignWithMargins: Boolean;
    function GetActualScaleWithDocument: Boolean;
    function GetAssigned: Boolean;
    procedure SetAlignWithMargins(const Value: TdxDefaultBoolean);
    procedure SetCommonFooter(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure SetCommonHeader(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure SetEvenPagesFooter(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure SetEvenPagesHeader(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure SetFirstPageFooter(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure SetFirstPageHeader(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure SetScaleWithDocument(const Value: TdxDefaultBoolean);
  public
    constructor Create(AChangeHandler: TNotifyEvent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;

    property Assigned: Boolean read GetAssigned;
    property ActualAlignWithMargins: Boolean read GetActualAlignWithMargins;
    property ActualScaleWithDocument: Boolean read GetActualScaleWithDocument;
  published
    property AlignWithMargins: TdxDefaultBoolean read FAlignWithMargins write SetAlignWithMargins default bDefault;
    property ScaleWithDocument: TdxDefaultBoolean read FScaleWithDocument write SetScaleWithDocument default bDefault;
    property CommonFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText read FCommonFooter write SetCommonFooter;
    property CommonHeader: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText read FCommonHeader write SetCommonHeader;
    property EvenPagesFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText read FEvenPagesFooter write SetEvenPagesFooter;
    property EvenPagesHeader: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText read FEvenPagesHeader write SetEvenPagesHeader;
    property FirstPageFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText read FFirstPageFooter write SetFirstPageFooter;
    property FirstPageHeader: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText read FFirstPageHeader write SetFirstPageHeader;
  end;

  { TdxSpreadSheetTableViewOptionsPrintRect }

  TdxSpreadSheetTableViewOptionsPrintRect = class(TcxRect)
  strict private
    FAssigned: Boolean;

    procedure SetAssigned(AValue: Boolean);
  protected
    procedure DoChange; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
  published
    property Assigned: Boolean read FAssigned write SetAssigned default False;
  end;

  { TdxSpreadSheetTableViewOptionsPrintPagePaper }

  TdxSpreadSheetTableViewOptionsPrintPagePaper = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  strict private
    FAssigned: Boolean;
    FCustomSize: TdxPointDoublePersistent;
    FSizeID: Integer;

    procedure CustomSizeChangeHandler(Sender: TObject);
    procedure SetAssigned(AValue: Boolean);
    procedure SetCustomSize(AValue: TdxPointDoublePersistent);
    procedure SetSizeID(AValue: Integer);
  public
    constructor Create(AChangeHandler: TNotifyEvent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Assigned: Boolean read FAssigned write SetAssigned default False;
    property CustomSize: TdxPointDoublePersistent read FCustomSize write SetCustomSize;
    property SizeID: Integer read FSizeID write SetSizeID default 0;
  end;

  { TdxSpreadSheetTableViewOptionsPrintPageMargins }

  TdxSpreadSheetTableViewOptionsPrintPageMargins = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  strict private const
    ValuesCount = 6;
  strict private
    FAssigned: Boolean;
    FValues: array[0..ValuesCount - 1] of Double;

    function GetValue(AIndex: Integer): Double;
    function IsValueStored(AIndex: Integer): Boolean;
    procedure SetAssigned(AValue: Boolean);
    procedure SetValue(AIndex: Integer; const AValue: Double);
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Assigned: Boolean read FAssigned write SetAssigned default False;
    property Footer: Double index 0 read GetValue write SetValue stored IsValueStored;
    property Header: Double index 1 read GetValue write SetValue stored IsValueStored;
    property Bottom: Double index 2 read GetValue write SetValue stored IsValueStored;
    property Left: Double index 3 read GetValue write SetValue stored IsValueStored;
    property Right: Double index 4 read GetValue write SetValue stored IsValueStored;
    property Top: Double index 5 read GetValue write SetValue stored IsValueStored;
  end;

  { TdxSpreadSheetTableViewOptionsPrintPage }

  TdxSpreadSheetTableViewOptionsPrintPageOrientation = (oppoDefault, oppoLandscape, oppoPortrait);
  TdxSpreadSheetTableViewOptionsPrintPageScaleMode = (oppsmDefault, oppsmAdjustToScale, oppsmFitToPage);

  TdxSpreadSheetTableViewOptionsPrintPage = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  public const
    ScaleMin = 10;
    ScaleMax = 400;
  strict private
    FFirstPageNumber: Cardinal;
    FFitToHeight: Cardinal;
    FFitToWidth: Cardinal;
    FMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins;
    FOrientation: TdxSpreadSheetTableViewOptionsPrintPageOrientation;
    FPaper: TdxSpreadSheetTableViewOptionsPrintPagePaper;
    FScale: Integer;
    FScaleMode: TdxSpreadSheetTableViewOptionsPrintPageScaleMode;

    procedure SetFitToHeight(AValue: Cardinal);
    procedure SetFitToWidth(AValue: Cardinal);
    procedure SetMargins(AValue: TdxSpreadSheetTableViewOptionsPrintPageMargins);
    procedure SetPaper(AValue: TdxSpreadSheetTableViewOptionsPrintPagePaper);
    procedure SetScale(AValue: Integer);
  private
    procedure SetFirstPageNumber(const Value: Cardinal);
    procedure SetOrientation(const Value: TdxSpreadSheetTableViewOptionsPrintPageOrientation);
    procedure SetScaleMode(const Value: TdxSpreadSheetTableViewOptionsPrintPageScaleMode);
  public
    constructor Create(AChangeHandler: TNotifyEvent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property FirstPageNumber: Cardinal read FFirstPageNumber write SetFirstPageNumber default 0;
    property FitToHeight: Cardinal read FFitToHeight write SetFitToHeight default 0;
    property FitToWidth: Cardinal read FFitToWidth write SetFitToWidth default 0;
    property Margins: TdxSpreadSheetTableViewOptionsPrintPageMargins read FMargins write SetMargins;
    property Orientation: TdxSpreadSheetTableViewOptionsPrintPageOrientation read FOrientation write SetOrientation default oppoDefault;
    property Paper: TdxSpreadSheetTableViewOptionsPrintPagePaper read FPaper write SetPaper;
    property Scale: Integer read FScale write SetScale default 100;
    property ScaleMode: TdxSpreadSheetTableViewOptionsPrintPageScaleMode read FScaleMode write SetScaleMode default oppsmDefault;
  end;

  { TdxSpreadSheetTableViewOptionsPrintPagination }

  TdxSpreadSheetTableViewOptionsPrintPagination = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  strict private
    FColumnPageBreaks: TList<Cardinal>;
    FRowPageBreaks: TList<Cardinal>;

    procedure ChangeHandler(Sender: TObject; const Item: Cardinal; Action: TCollectionNotification);
    procedure SetColumnPageBreaks(AValue: TList<Cardinal>);
    procedure SetRowPageBreaks(AValue: TList<Cardinal>);
  public
    constructor Create(AChangeHandler: TNotifyEvent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;

    property ColumnPageBreaks: TList<Cardinal> read FColumnPageBreaks write SetColumnPageBreaks;
    property RowPageBreaks: TList<Cardinal> read FRowPageBreaks write SetRowPageBreaks;
  end;

  { TdxSpreadSheetTableViewOptionsPrintPrinting }

  TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder = (opppDefault, opppDownThenOver, opppOverThenDown);

  TdxSpreadSheetTableViewOptionsPrintPrinting = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  strict private
    FBlackAndWhite: TdxDefaultBoolean;
    FCopies: Cardinal;
    FDraft: TdxDefaultBoolean;
    FHorizontalCentered: TdxDefaultBoolean;
    FPageOrder: TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder;
    FVerticalCentered: TdxDefaultBoolean;

    function GetActualBlackAndWhite: Boolean;
    function GetActualDraft: Boolean;
    function GetActualHorizontalCentered: Boolean;
    function GetActualPageOrder: TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder;
    function GetActualVerticalCentered: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    //
    property ActualBlackAndWhite: Boolean read GetActualBlackAndWhite;
    property ActualDraft: Boolean read GetActualDraft;
    property ActualHorizontalCentered: Boolean read GetActualHorizontalCentered;
    property ActualPageOrder: TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder read GetActualPageOrder;
    property ActualVerticalCentered: Boolean read GetActualVerticalCentered;
  published
    property BlackAndWhite: TdxDefaultBoolean read FBlackAndWhite write FBlackAndWhite default bDefault;
    property Copies: Cardinal read FCopies write FCopies default 0;
    property Draft: TdxDefaultBoolean read FDraft write FDraft default bDefault;
    property HorizontalCentered: TdxDefaultBoolean read FHorizontalCentered write FHorizontalCentered default bDefault;
    property PageOrder: TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder read FPageOrder write FPageOrder default opppDefault;
    property VerticalCentered: TdxDefaultBoolean read FVerticalCentered write FVerticalCentered default bDefault;
  end;

  { TdxSpreadSheetTableViewOptionsPrintSource }

  TdxSpreadSheetTableViewOptionsPrintSourceCellComments = (psccAsDisplayed, psccAtEnd, psccNone);
  TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication = (pseiDefault, pseiBlank, pseiDash, pseiDisplayText, pseiNA);

  TdxSpreadSheetTableViewOptionsPrintSource = class(TdxSpreadSheetTableViewOptionsPrintCustomPersistent)
  strict private
    FCellComments: TdxSpreadSheetTableViewOptionsPrintSourceCellComments;
    FArea: TdxSpreadSheetTableViewOptionsPrintRect;
    FColumnsToRepeat: TdxSpreadSheetTableViewOptionsPrintRect;
    FErrorIndication: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication;
    FGridLines: TdxDefaultBoolean;
    FHeaders: TdxDefaultBoolean;
    FRowsToRepeat: TdxSpreadSheetTableViewOptionsPrintRect;

    function GetActualErrorIndication: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication;
    function GetActualGridLines: Boolean;
    function GetActualHeaders: Boolean;
    procedure SetArea(AValue: TdxSpreadSheetTableViewOptionsPrintRect);
    procedure SetCellComments(const Value: TdxSpreadSheetTableViewOptionsPrintSourceCellComments);
    procedure SetColumnsToRepeat(AValue: TdxSpreadSheetTableViewOptionsPrintRect);
    procedure SetErrorIndication(const Value: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication);
    procedure SetGridLines(const Value: TdxDefaultBoolean);
    procedure SetHeaders(const Value: TdxDefaultBoolean);
    procedure SetRowsToRepeat(AValue: TdxSpreadSheetTableViewOptionsPrintRect);
  public
    constructor Create(AChangeHandler: TNotifyEvent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;

    property ActualErrorIndication: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication read GetActualErrorIndication;
    property ActualGridLines: Boolean read GetActualGridLines;
    property ActualHeaders: Boolean read GetActualHeaders;
  published
    property Area: TdxSpreadSheetTableViewOptionsPrintRect read FArea write SetArea;
    property CellComments: TdxSpreadSheetTableViewOptionsPrintSourceCellComments read FCellComments write SetCellComments default psccNone;
    property ColumnsToRepeat: TdxSpreadSheetTableViewOptionsPrintRect read FColumnsToRepeat write SetColumnsToRepeat;
    property ErrorIndication: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication read FErrorIndication write SetErrorIndication default pseiDefault;
    property GridLines: TdxDefaultBoolean read FGridLines write SetGridLines default bDefault;
    property Headers: TdxDefaultBoolean read FHeaders write SetHeaders default bDefault;
    property RowsToRepeat: TdxSpreadSheetTableViewOptionsPrintRect read FRowsToRepeat write SetRowsToRepeat;
  end;

  { TdxSpreadSheetAbstractHeaderFooterMacroExpander }

  TdxSpreadSheetAbstractHeaderFooterMacroExpander = class
  strict private type
    TEvalFunc = function (const S: string; var AIndex: Integer; ALength: Integer): string of object;
    TEvalFuncMap = TDictionary<string, TEvalFunc>;
  strict private
    class var FInstances: TDictionary<TClass, TEvalFuncMap>;

    class procedure GetEvalFunctions(out AEvalFuncs: TEvalFuncMap);
  protected
    class var FFont: TFont;

    // Evaluate Functions
    class function EvalFuncAmpersand(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncDate(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncFont(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncFontColor(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncFontSize(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncFontStrikeOut(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncFontUnderline(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncPageNumber(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncPages(const S: string;var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncSheetName(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    class function EvalFuncTime(const S: string; var AIndex: Integer; ALength: Integer): string; virtual;
    // Main
    class function EvaluateCore(const S: string): string;
    // Utils
    class procedure RegisterFunctions(AEvalFuncs: TEvalFuncMap); virtual;
  public
    class procedure Finalize;
  end;

implementation

uses
  SysUtils, Math, StrUtils, cxGraphics, dxStringHelper;

{ TdxSpreadSheetTableViewOptionsPrintCustomPersistent }

constructor TdxSpreadSheetTableViewOptionsPrintCustomPersistent.Create(AChangeHandler: TNotifyEvent);
begin
  FOnChange := AChangeHandler;
end;

procedure TdxSpreadSheetTableViewOptionsPrintCustomPersistent.AfterConstruction;
begin
  inherited AfterConstruction;
  Inc(FChangeLockCount);
  try
    Reset;
  finally
    Dec(FChangeLockCount);
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintCustomPersistent.Changed;
begin
  if FChangeLockCount = 0 then
    dxCallNotify(OnChange, Self);
end;

procedure TdxSpreadSheetTableViewOptionsPrintCustomPersistent.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

{ TdxSpreadSheetTableViewOptionsPrintHeaderFooterText }

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintHeaderFooterText then
  begin
    for I := 0 to SectionCount - 1 do
      SetValue(I, TdxSpreadSheetTableViewOptionsPrintHeaderFooterText(Source).GetValue(I));
    Assigned := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText(Source).Assigned;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Reset;
var
  I: Integer;
begin
  for I := 0 to SectionCount - 1 do
    SetValue(I, '');
  Assigned := False;
end;

function TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.GetValue(const Index: Integer): string;
begin
  Result := FValues[Index];
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.SetAssigned(const Value: Boolean);
begin
  if FAssigned <> Value then
  begin
    FAssigned := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.SetValue(const Index: Integer; const Value: string);
begin
  FValues[Index] := Value;
  Assigned := True;
  Changed;
end;

{ TdxSpreadSheetTableViewOptionsPrintHeaderFooter }

constructor TdxSpreadSheetTableViewOptionsPrintHeaderFooter.Create(AChangeHandler: TNotifyEvent);
begin
  inherited Create(AChangeHandler);
  FCommonHeader := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(ChangeHandler);
  FCommonFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(ChangeHandler);
  FEvenPagesHeader := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(ChangeHandler);
  FEvenPagesFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(ChangeHandler);
  FFirstPageHeader := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(ChangeHandler);
  FFirstPageFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(ChangeHandler);
end;

destructor TdxSpreadSheetTableViewOptionsPrintHeaderFooter.Destroy;
begin
  FreeAndNil(FCommonFooter);
  FreeAndNil(FCommonHeader);
  FreeAndNil(FEvenPagesFooter);
  FreeAndNil(FEvenPagesHeader);
  FreeAndNil(FFirstPageFooter);
  FreeAndNil(FFirstPageHeader);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintHeaderFooter then
  begin
    AlignWithMargins := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).AlignWithMargins;
    ScaleWithDocument := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).ScaleWithDocument;
    CommonFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).CommonFooter;
    CommonHeader := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).CommonHeader;
    EvenPagesFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).EvenPagesFooter;
    EvenPagesHeader := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).EvenPagesHeader;
    FirstPageFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).FirstPageFooter;
    FirstPageHeader := TdxSpreadSheetTableViewOptionsPrintHeaderFooter(Source).FirstPageHeader;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.Reset;
begin
  AlignWithMargins := bDefault;
  ScaleWithDocument := bDefault;
  CommonFooter.Reset;
  CommonHeader.Reset;
  EvenPagesFooter.Reset;
  EvenPagesHeader.Reset;
  FirstPageFooter.Reset;
  FirstPageHeader.Reset;
end;

function TdxSpreadSheetTableViewOptionsPrintHeaderFooter.GetActualAlignWithMargins: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(AlignWithMargins, True);
end;

function TdxSpreadSheetTableViewOptionsPrintHeaderFooter.GetActualScaleWithDocument: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(ScaleWithDocument, True);
end;

function TdxSpreadSheetTableViewOptionsPrintHeaderFooter.GetAssigned: Boolean;
begin
  Result := (AlignWithMargins <> bDefault) or (ScaleWithDocument <> bDefault) or
    CommonFooter.Assigned or CommonHeader.Assigned or
    EvenPagesFooter.Assigned or EvenPagesHeader.Assigned or
    FirstPageFooter.Assigned or FirstPageFooter.Assigned;
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetAlignWithMargins(const Value: TdxDefaultBoolean);
begin
  if FAlignWithMargins <> Value then
  begin
    FAlignWithMargins := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetCommonFooter(
  AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  FCommonFooter.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetCommonHeader(
  AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  FCommonHeader.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetEvenPagesFooter(
  AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  FEvenPagesFooter.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetEvenPagesHeader(
  AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  FEvenPagesHeader.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetFirstPageFooter(
  AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  FFirstPageFooter.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetFirstPageHeader(
  AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  FEvenPagesHeader.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintHeaderFooter.SetScaleWithDocument(const Value: TdxDefaultBoolean);
begin
  if FScaleWithDocument <> Value then
  begin
    FScaleWithDocument := Value;
    Changed;
  end;
end;

{ TdxSpreadSheetTableViewOptionsPrintRect }

procedure TdxSpreadSheetTableViewOptionsPrintRect.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxSpreadSheetTableViewOptionsPrintRect then
    Assigned := TdxSpreadSheetTableViewOptionsPrintRect(Source).Assigned;
end;

procedure TdxSpreadSheetTableViewOptionsPrintRect.Reset;
begin
  Rect := cxNullRect;
  Assigned := False;
end;

procedure TdxSpreadSheetTableViewOptionsPrintRect.SetAssigned(AValue: Boolean);
begin
  if FAssigned <> AValue then
  begin
    FAssigned := AValue;
    inherited DoChange;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintRect.DoChange;
begin
  Assigned := True;
  inherited DoChange;
end;

{ TdxSpreadSheetTableViewOptionsPrintPagePaper }

constructor TdxSpreadSheetTableViewOptionsPrintPagePaper.Create(AChangeHandler: TNotifyEvent);
begin
  inherited Create(AChangeHandler);
  FCustomSize := TdxPointDoublePersistent.Create(nil);
  FCustomSize.OnChange := CustomSizeChangeHandler;
end;

destructor TdxSpreadSheetTableViewOptionsPrintPagePaper.Destroy;
begin
  FreeAndNil(FCustomSize);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagePaper.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintPagePaper then
  begin
    CustomSize := TdxSpreadSheetTableViewOptionsPrintPagePaper(Source).CustomSize;
    SizeID := TdxSpreadSheetTableViewOptionsPrintPagePaper(Source).SizeID;
    Assigned := TdxSpreadSheetTableViewOptionsPrintPagePaper(Source).Assigned; // last
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagePaper.Reset;
begin
  SizeID := 0;
  CustomSize.Reset;
  Assigned := False;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagePaper.CustomSizeChangeHandler(Sender: TObject);
begin
  Assigned := True;
  SizeID := 0;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagePaper.SetAssigned(AValue: Boolean);
begin
  if FAssigned <> AValue then
  begin
    FAssigned := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagePaper.SetCustomSize(AValue: TdxPointDoublePersistent);
begin
  FCustomSize.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagePaper.SetSizeID(AValue: Integer);
begin
  Assigned := True;
  if AValue <> DMPAPER_USER then
    AValue := Min(Max(AValue, 0), DMPAPER_LAST);
  FSizeID := AValue;
  Changed;
end;

{ TdxSpreadSheetTableViewOptionsPrintPagination }

constructor TdxSpreadSheetTableViewOptionsPrintPagination.Create(AChangeHandler: TNotifyEvent);
begin
  inherited;
  FRowPageBreaks := TList<Cardinal>.Create;
  FRowPageBreaks.OnNotify := ChangeHandler;
  FColumnPageBreaks := TList<Cardinal>.Create;
  FColumnPageBreaks.OnNotify := ChangeHandler;
end;

destructor TdxSpreadSheetTableViewOptionsPrintPagination.Destroy;
begin
  FreeAndNil(FColumnPageBreaks);
  FreeAndNil(FRowPageBreaks);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagination.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintPagination then
  begin
    ColumnPageBreaks := TdxSpreadSheetTableViewOptionsPrintPagination(Source).ColumnPageBreaks;
    RowPageBreaks := TdxSpreadSheetTableViewOptionsPrintPagination(Source).RowPageBreaks;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagination.Reset;
begin
  ColumnPageBreaks.Clear;
  RowPageBreaks.Clear;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagination.ChangeHandler(
  Sender: TObject; const Item: Cardinal; Action: TCollectionNotification);
begin
  Changed;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagination.SetColumnPageBreaks(AValue: TList<Cardinal>);
begin
  FColumnPageBreaks.Clear;
  FColumnPageBreaks.Capacity := AValue.Count;
  FColumnPageBreaks.AddRange(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintPagination.SetRowPageBreaks(AValue: TList<Cardinal>);
begin
  FRowPageBreaks.Clear;
  FRowPageBreaks.Capacity := AValue.Count;
  FRowPageBreaks.AddRange(AValue);
end;

{ TdxSpreadSheetTableViewOptionsPrintPageMargins }

procedure TdxSpreadSheetTableViewOptionsPrintPageMargins.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintPageMargins then
  begin
    for I := 0 to ValuesCount - 1 do
      FValues[I] := TdxSpreadSheetTableViewOptionsPrintPageMargins(Source).FValues[I];
    Assigned := TdxSpreadSheetTableViewOptionsPrintPageMargins(Source).Assigned;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPageMargins.Reset;
var
  I: Integer;
begin
  for I := 0 to ValuesCount - 1 do
    FValues[I] := 0;
  Assigned := False;
end;

function TdxSpreadSheetTableViewOptionsPrintPageMargins.GetValue(AIndex: Integer): Double;
begin
  Result := FValues[AIndex];
end;

function TdxSpreadSheetTableViewOptionsPrintPageMargins.IsValueStored(AIndex: Integer): Boolean;
begin
  Result := not IsZero(GetValue(AIndex));
end;

procedure TdxSpreadSheetTableViewOptionsPrintPageMargins.SetAssigned(AValue: Boolean);
begin
  if FAssigned <> AValue then
  begin
    FAssigned := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPageMargins.SetValue(AIndex: Integer; const AValue: Double);
begin
  Assigned := True;
  FValues[AIndex] := AValue;
  Changed;
end;

{ TdxSpreadSheetTableViewOptionsPrintPage }

constructor TdxSpreadSheetTableViewOptionsPrintPage.Create(AChangeHandler: TNotifyEvent);
begin
  inherited;
  FMargins := TdxSpreadSheetTableViewOptionsPrintPageMargins.Create(ChangeHandler);
  FPaper := TdxSpreadSheetTableViewOptionsPrintPagePaper.Create(ChangeHandler);
end;

destructor TdxSpreadSheetTableViewOptionsPrintPage.Destroy;
begin
  FreeAndNil(FMargins);
  FreeAndNil(FPaper);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintPage then
  begin
    FitToHeight := TdxSpreadSheetTableViewOptionsPrintPage(Source).FitToHeight;
    FitToWidth := TdxSpreadSheetTableViewOptionsPrintPage(Source).FitToWidth;
    Orientation := TdxSpreadSheetTableViewOptionsPrintPage(Source).Orientation;
    FirstPageNumber := TdxSpreadSheetTableViewOptionsPrintPage(Source).FirstPageNumber;
    Margins := TdxSpreadSheetTableViewOptionsPrintPage(Source).Margins;
    Paper := TdxSpreadSheetTableViewOptionsPrintPage(Source).Paper;
    Scale := TdxSpreadSheetTableViewOptionsPrintPage(Source).Scale;
    ScaleMode := TdxSpreadSheetTableViewOptionsPrintPage(Source).ScaleMode;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.Reset;
begin
  Scale := 100;
  FitToHeight := 0;
  FitToWidth := 0;
  ScaleMode := oppsmDefault;
  Orientation := oppoDefault;
  FirstPageNumber := 0;
  Paper.Reset;
  Margins.Reset;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetFirstPageNumber(const Value: Cardinal);
begin
  if FFirstPageNumber <> Value then
  begin
    FFirstPageNumber := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetFitToHeight(AValue: Cardinal);
begin
  FFitToHeight := AValue;
  FScaleMode := oppsmFitToPage;
  Changed;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetFitToWidth(AValue: Cardinal);
begin
  FFitToWidth := AValue;
  FScaleMode := oppsmFitToPage;
  Changed;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetMargins(AValue: TdxSpreadSheetTableViewOptionsPrintPageMargins);
begin
  FMargins.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetOrientation(
  const Value: TdxSpreadSheetTableViewOptionsPrintPageOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetPaper(AValue: TdxSpreadSheetTableViewOptionsPrintPagePaper);
begin
  FPaper.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetScale(AValue: Integer);
begin
  FScale := Min(Max(AValue, ScaleMin), ScaleMax);
  FScaleMode := oppsmAdjustToScale;
  Changed;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPage.SetScaleMode(
  const Value: TdxSpreadSheetTableViewOptionsPrintPageScaleMode);
begin
  if FScaleMode <> Value then
  begin
    FScaleMode := Value;
    Changed;
  end;
end;

{ TdxSpreadSheetTableViewOptionsPrintPrinting }

procedure TdxSpreadSheetTableViewOptionsPrintPrinting.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintPrinting then
  begin
    Draft := TdxSpreadSheetTableViewOptionsPrintPrinting(Source).Draft;
    BlackAndWhite := TdxSpreadSheetTableViewOptionsPrintPrinting(Source).BlackAndWhite;
    Copies := TdxSpreadSheetTableViewOptionsPrintPrinting(Source).Copies;
    HorizontalCentered := TdxSpreadSheetTableViewOptionsPrintPrinting(Source).HorizontalCentered;
    PageOrder := TdxSpreadSheetTableViewOptionsPrintPrinting(Source).PageOrder;
    VerticalCentered := TdxSpreadSheetTableViewOptionsPrintPrinting(Source).VerticalCentered;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintPrinting.Reset;
begin
  BlackAndWhite := bDefault;
  Copies := 0;
  HorizontalCentered := bDefault;
  VerticalCentered := bDefault;
  PageOrder := opppDefault;
  Draft := bDefault;
end;

function TdxSpreadSheetTableViewOptionsPrintPrinting.GetActualBlackAndWhite: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(BlackAndWhite, False);
end;

function TdxSpreadSheetTableViewOptionsPrintPrinting.GetActualDraft: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(Draft, False);
end;

function TdxSpreadSheetTableViewOptionsPrintPrinting.GetActualHorizontalCentered: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(HorizontalCentered, False);
end;

function TdxSpreadSheetTableViewOptionsPrintPrinting.GetActualPageOrder: TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder;
begin
  if PageOrder <> opppDefault then
    Result := PageOrder
  else
    Result := opppDownThenOver;
end;

function TdxSpreadSheetTableViewOptionsPrintPrinting.GetActualVerticalCentered: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(VerticalCentered, False);
end;

{ TdxSpreadSheetTableViewOptionsPrintSource }

constructor TdxSpreadSheetTableViewOptionsPrintSource.Create(AChangeHandler: TNotifyEvent);
begin
  inherited;
  FArea := TdxSpreadSheetTableViewOptionsPrintRect.Create(Self);
  FArea.OnChange := ChangeHandler;
  FRowsToRepeat := TdxSpreadSheetTableViewOptionsPrintRect.Create(Self);
  FRowsToRepeat.OnChange := ChangeHandler;
  FColumnsToRepeat := TdxSpreadSheetTableViewOptionsPrintRect.Create(Self);
  FColumnsToRepeat.OnChange := ChangeHandler;
end;

destructor TdxSpreadSheetTableViewOptionsPrintSource.Destroy;
begin
  FreeAndNil(FColumnsToRepeat);
  FreeAndNil(FRowsToRepeat);
  FreeAndNil(FArea);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetTableViewOptionsPrintSource then
  begin
    Area := TdxSpreadSheetTableViewOptionsPrintSource(Source).Area;
    RowsToRepeat := TdxSpreadSheetTableViewOptionsPrintSource(Source).RowsToRepeat;
    CellComments := TdxSpreadSheetTableViewOptionsPrintSource(Source).CellComments;
    ColumnsToRepeat := TdxSpreadSheetTableViewOptionsPrintSource(Source).ColumnsToRepeat;
    ErrorIndication := TdxSpreadSheetTableViewOptionsPrintSource(Source).ErrorIndication;
    GridLines := TdxSpreadSheetTableViewOptionsPrintSource(Source).GridLines;
    Headers := TdxSpreadSheetTableViewOptionsPrintSource(Source).Headers;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.Reset;
begin
  Area.Reset;
  RowsToRepeat.Reset;
  ColumnsToRepeat.Reset;
  CellComments := psccNone;
  ErrorIndication := pseiDefault;
  GridLines := bDefault;
  Headers := bDefault;
end;

function TdxSpreadSheetTableViewOptionsPrintSource.GetActualErrorIndication: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication;
begin
  Result := ErrorIndication;
  if Result = pseiDefault then
    Result := pseiDisplayText;
end;

function TdxSpreadSheetTableViewOptionsPrintSource.GetActualGridLines: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(GridLines, False);
end;

function TdxSpreadSheetTableViewOptionsPrintSource.GetActualHeaders: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(Headers, False);
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.SetArea(AValue: TdxSpreadSheetTableViewOptionsPrintRect);
begin
  FArea.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.SetCellComments(
  const Value: TdxSpreadSheetTableViewOptionsPrintSourceCellComments);
begin
  if FCellComments <> Value then
  begin
    FCellComments := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.SetColumnsToRepeat(AValue: TdxSpreadSheetTableViewOptionsPrintRect);
begin
  FColumnsToRepeat.Assign(AValue);
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.SetErrorIndication(
  const Value: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication);
begin
  if FErrorIndication <> Value then
  begin
    FErrorIndication := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.SetGridLines(const Value: TdxDefaultBoolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.SetHeaders(const Value: TdxDefaultBoolean);
begin
  if FHeaders <> Value then
  begin
    FHeaders := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetTableViewOptionsPrintSource.SetRowsToRepeat(AValue: TdxSpreadSheetTableViewOptionsPrintRect);
begin
  FRowsToRepeat.Assign(AValue);
end;

{ TdxSpreadSheetAbstractHeaderFooterMacroExpander }

class procedure TdxSpreadSheetAbstractHeaderFooterMacroExpander.Finalize;
begin
  FreeAndNil(FInstances);
end;

class procedure TdxSpreadSheetAbstractHeaderFooterMacroExpander.RegisterFunctions(AEvalFuncs: TEvalFuncMap);
begin
  AEvalFuncs.Add('"', EvalFuncFont);
  AEvalFuncs.Add('&', EvalFuncAmpersand);
  AEvalFuncs.Add('1', EvalFuncFontSize);
  AEvalFuncs.Add('2', EvalFuncFontSize);
  AEvalFuncs.Add('3', EvalFuncFontSize);
  AEvalFuncs.Add('4', EvalFuncFontSize);
  AEvalFuncs.Add('5', EvalFuncFontSize);
  AEvalFuncs.Add('6', EvalFuncFontSize);
  AEvalFuncs.Add('7', EvalFuncFontSize);
  AEvalFuncs.Add('8', EvalFuncFontSize);
  AEvalFuncs.Add('9', EvalFuncFontSize);
  AEvalFuncs.Add('A', EvalFuncSheetName);
  AEvalFuncs.Add('D', EvalFuncDate);
  AEvalFuncs.Add('E', EvalFuncFontUnderline); // double underline
  AEvalFuncs.Add('K', EvalFuncFontColor);
  AEvalFuncs.Add('N', EvalFuncPages);
  AEvalFuncs.Add('P', EvalFuncPageNumber);
  AEvalFuncs.Add('S', EvalFuncFontStrikeOut);
  AEvalFuncs.Add('T', EvalFuncTime);
  AEvalFuncs.Add('U', EvalFuncFontUnderline);

end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncAmpersand(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := '&';
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncDate(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  raise Exception.Create('not implemented');
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncFont(
  const S: string; var AIndex: Integer; ALength: Integer): string;
var
  APos: Integer;
begin
  APos := PosEx('"', S, AIndex);
  if APos > 0 then
  begin
    AIndex := APos + 1;
  end;
  Result := '';
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncFontColor(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  if FFont <> nil then
    FFont.Color := TdxColorHelper.HexCodeToAlphaColor(Copy(S, AIndex, 6), False);
  Inc(AIndex, 6);
  Result := '';
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncFontSize(
  const S: string; var AIndex: Integer; ALength: Integer): string;
var
  APos: Integer;
begin
  APos := AIndex;
  while APos <= ALength do
  begin
    if not dxCharIsNumeric(S[APos]) then
    begin
      ExchangeLongWords(APos, AIndex);
      if FFont <> nil then
        FFont.Size := StrToInt(Copy(S, APos, AIndex - APos));
      Break;
    end;
    Inc(APos);
  end;
  Result := '';
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncFontStrikeOut(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  if FFont <> nil then
    FFont.Style := [fsStrikeOut];
  Result := '';
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncFontUnderline(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  if FFont <> nil then
    FFont.Style := [fsUnderline];
  Result := '';
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncPageNumber(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  raise Exception.Create('not implemented');
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncPages(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  raise Exception.Create('not implemented');
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncSheetName(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  raise Exception.Create('not implemented');
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvalFuncTime(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  raise Exception.Create('not implemented');
end;

class function TdxSpreadSheetAbstractHeaderFooterMacroExpander.EvaluateCore(const S: string): string;
var
  ABuilder: TStringBuilder;
  AFunc: TEvalFunc;
  AFuncs: TEvalFuncMap;
  AIndex: Integer;
  ALength: Integer;
begin
  if S = '' then
    Exit(S);

  ALength := Length(S);
  ABuilder := TdxStringBuilderManager.Get(ALength);
  try
    AIndex := 1;
    GetEvalFunctions(AFuncs);
    while AIndex <= ALength do
    begin
      if (S[AIndex] = '&') and (AIndex < ALength) then
      begin
        Inc(AIndex);
        if AFuncs.TryGetValue(S[AIndex], AFunc) then
        begin
          Inc(AIndex);
          ABuilder.Append(AFunc(S, AIndex, ALength));
        end
        else
          Inc(AIndex);
      end
      else
      begin
        ABuilder.Append(S[AIndex]);
        Inc(AIndex);
      end;
    end;
    Result := ABuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

class procedure TdxSpreadSheetAbstractHeaderFooterMacroExpander.GetEvalFunctions(out AEvalFuncs: TEvalFuncMap);
begin
  if FInstances = nil then
    FInstances := TObjectDictionary<TClass, TEvalFuncMap>.Create([doOwnsValues]);
  if not FInstances.TryGetValue(Self, AEvalFuncs) then
  begin
    AEvalFuncs := TEvalFuncMap.Create;
    RegisterFunctions(AEvalFuncs);
    FInstances.Add(Self, AEvalFuncs);
  end;
end;

initialization

finalization
  TdxSpreadSheetAbstractHeaderFooterMacroExpander.Finalize;
end.
