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

unit dxSpreadSheetStyles;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, Graphics,
  dxCore, dxCoreClasses, cxGraphics, dxHashUtils, cxClasses, cxFormats, cxVariants,
  dxSpreadSheetTypes,
  dxSpreadSheetClasses,
  dxSpreadSheetCoreStyles,
  dxSpreadSheetGraphics,
  dxSpreadSheetNumberFormat;

type
  TdxSpreadSheetCellStyle = class;

  TdxSpreadSheetCellStyleEnumProcRef = reference to procedure (
    ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect);

  { IdxSpreadSheetCellStyleOwner }

  IdxSpreadSheetCellStyleOwner = interface
  ['{7E2D8A63-43AE-401B-B98C-9BB737DB60CF}']
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;

    procedure CellStyleChanged;
    procedure CellStyleChanging;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
    function GetCellStyles: TdxSpreadSheetCellStyles;

    property FormatSettings: TdxSpreadSheetCustomFormatSettings read GetFormatSettings;
  end;

  { TdxSpreadSheetCellBorder }

  TdxSpreadSheetCellBorder = class
  strict private
    FKind: TcxBorder;
    FOwner: TdxSpreadSheetCellStyle;

    function GetColor: TColor;
    function GetHandle: TdxSpreadSheetBordersHandle;
    function GetStyle: TdxSpreadSheetCellBorderStyle;
    procedure SetColor(const AValue: TColor);
    procedure SetHandle(const AValue: TdxSpreadSheetBordersHandle);
    procedure SetStyle(const AValue: TdxSpreadSheetCellBorderStyle);
  protected
    procedure ChangeBorder(AStyle: TdxSpreadSheetCellBorderStyle; AColor: TColor);
    //
    property Handle: TdxSpreadSheetBordersHandle read GetHandle write SetHandle;
    property Kind: TcxBorder read FKind;
  public
    constructor Create(AOwner: TdxSpreadSheetCellStyle; AKind: TcxBorder); reintroduce;
    procedure Assign(ASource: TdxSpreadSheetCellBorder); overload; virtual;
    procedure Assign(ASource: TdxSpreadSheetBordersHandle); overload; virtual;
    procedure Reset;

    property Color: TColor read GetColor write SetColor;
    property Owner: TdxSpreadSheetCellStyle read FOwner;
    property Style: TdxSpreadSheetCellBorderStyle read GetStyle write SetStyle;
  end;

  { TdxSpreadSheetCellBrush }

  TdxSpreadSheetCellBrush = class(TdxSpreadSheetCustomBrush)
  strict private
    FOwner: TdxSpreadSheetCellStyle;
  protected
    function GetHandle: TdxSpreadSheetBrushHandle; override;
    procedure SetHandle(const AValue: TdxSpreadSheetBrushHandle); override;
  public
    constructor Create(AOwner: TdxSpreadSheetCellStyle);
    //
    property Owner: TdxSpreadSheetCellStyle read FOwner;
  end;

  { TdxSpreadSheetCellDataFormat }

  TdxSpreadSheetCellDataFormat = class(TdxSpreadSheetCustomDataFormat)
  strict private
    FOwner: TdxSpreadSheetCellStyle;

    function GetIsDateTime: Boolean;
    function GetIsText: Boolean;
    function GetIsTime: Boolean;
  protected
    function GetHandle: TdxSpreadSheetFormatHandle; override;
    procedure SetHandle(AValue: TdxSpreadSheetFormatHandle); override;
  public
    constructor Create(AOwner: TdxSpreadSheetCellStyle); reintroduce;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType; var AResult: TdxSpreadSheetNumberFormatResult);
    //
    property IsDateTime: Boolean read GetIsDateTime;
    property IsText: Boolean read GetIsText;
    property IsTime: Boolean read GetIsTime;
    //
    property Owner: TdxSpreadSheetCellStyle read FOwner;
  end;

  { TdxSpreadSheetCellFont }

  TdxSpreadSheetCellFont = class(TdxSpreadSheetCustomFont)
  strict private
    FOwner: TdxSpreadSheetCellStyle;
  protected
    function GetHandle: TdxSpreadSheetFontHandle; override;
    procedure SetHandle(const AValue: TdxSpreadSheetFontHandle); override;
  public
    constructor Create(AOwner: TdxSpreadSheetCellStyle); reintroduce;
    //
    property Owner: TdxSpreadSheetCellStyle read FOwner;
  end;

  { TdxSpreadSheetCellStyle }

  TdxSpreadSheetCellStyle = class
  strict private
    FBorders: array[TcxBorder] of TdxSpreadSheetCellBorder;
    FBrush: TdxSpreadSheetCellBrush;
    FDataFormat: TdxSpreadSheetCellDataFormat;
    FFont: TdxSpreadSheetCellFont;
    FOwner: IdxSpreadSheetCellStyleOwner;
    FPrevHandle: TdxSpreadSheetCellStyleHandle;
    FUpdateLockCount: Integer;

    procedure ChangeState(AState: TdxSpreadSheetCellState; AValue: Boolean);
    function GetAlignHorz: TdxSpreadSheetDataAlignHorz;
    function GetAlignHorzIndent: Integer;
    function GetAlignVert: TdxSpreadSheetDataAlignVert;
    function GetBorder(ABorder: TcxBorder): TdxSpreadSheetCellBorder;
    function GetBrush: TdxSpreadSheetCellBrush;
    function GetCellStyles: TdxSpreadSheetCellStyles; inline;
    function GetDataFormat: TdxSpreadSheetCellDataFormat; inline;
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
    function GetIsDefault: Boolean;
    function GetFont: TdxSpreadSheetCellFont;
    function GetHidden: Boolean;
    function GetLocked: Boolean;
    function GetShrinkToFit: Boolean;
    function GetState: TdxSpreadSheetCellStates;
    function GetWordWrap: Boolean;
    procedure SetAlignHorz(const AValue: TdxSpreadSheetDataAlignHorz);
    procedure SetAlignHorzIndent(const AValue: Integer);
    procedure SetAlignVert(const AValue: TdxSpreadSheetDataAlignVert);
    procedure SetBorder(ABorder: TcxBorder; const AValue: TdxSpreadSheetCellBorder);
    procedure SetBrush(const AValue: TdxSpreadSheetCellBrush);
    procedure SetDataFormat(const AValue: TdxSpreadSheetCellDataFormat);
    procedure SetFont(const AValue: TdxSpreadSheetCellFont);
    procedure SetHidden(const AValue: Boolean);
    procedure SetIsDefault(const Value: Boolean);
    procedure SetLocked(const AValue: Boolean);
    procedure SetShrinkToFit(const AValue: Boolean);
    procedure SetState(const AValue: TdxSpreadSheetCellStates);
    procedure SetWordWrap(const AValue: Boolean);
  protected
    FHandle: TdxSpreadSheetCellStyleHandle;

    procedure Changed;
    procedure CloneHandle; virtual;
    procedure DoChanged; virtual;
    procedure SetHandle(const AHandle: TdxSpreadSheetCellStyleHandle); virtual;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle); virtual;
    procedure ReleaseHandle; virtual;
    procedure ReplaceHandle; virtual;

    property CellStyles: TdxSpreadSheetCellStyles read GetCellStyles;
    property FormatSettings: TdxSpreadSheetCustomFormatSettings read GetFormatSettings;
    property Owner: IdxSpreadSheetCellStyleOwner read FOwner;
    property State: TdxSpreadSheetCellStates read GetState write SetState;
    property UpdateLockCount: Integer read FUpdateLockCount;
  public
    constructor Create(const AOwner: IdxSpreadSheetCellStyleOwner); overload; virtual;
    constructor Create(const AOwner: IdxSpreadSheetCellStyleOwner; AHandle: TdxSpreadSheetCellStyleHandle); overload; virtual;
    destructor Destroy; override;
    procedure Assign(AStyle: TdxSpreadSheetCellStyle);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Merge(AColumnStyle, ARowStyle: TdxSpreadSheetCellStyle);

    property AlignHorz: TdxSpreadSheetDataAlignHorz read GetAlignHorz write SetAlignHorz;
    property AlignHorzIndent: Integer read GetAlignHorzIndent write SetAlignHorzIndent;
    property AlignVert: TdxSpreadSheetDataAlignVert read GetAlignVert write SetAlignVert;
    property Borders[ABorder: TcxBorder]: TdxSpreadSheetCellBorder read GetBorder write SetBorder;
    property Brush: TdxSpreadSheetCellBrush read GetBrush write SetBrush;
    property DataFormat: TdxSpreadSheetCellDataFormat read GetDataFormat write SetDataFormat;
    property Font: TdxSpreadSheetCellFont read GetFont write SetFont;
    property Hidden: Boolean read GetHidden write SetHidden;
    property Locked: Boolean read GetLocked write SetLocked;
    property ShrinkToFit: Boolean read GetShrinkToFit write SetShrinkToFit;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
    //
    property Handle: TdxSpreadSheetCellStyleHandle read FHandle write SetHandle;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
  end;

  { TdxSpreadSheetCellDataBar }

  TdxSpreadSheetCellDataBar = class(TPersistent)
  strict private
    FAxisColor: TColor;
    FAxisPosition: Single;
    FBorder: TColor;
    FColor1: TColor;
    FColor2: TColor;
    FPosition: Single;
  protected
    procedure ReadData(AReader: TReader);
    procedure WriteData(AWriter: TWriter);
  public
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    function IsEmpty: Boolean;
    procedure Reset;

    property AxisColor: TColor read FAxisColor write FAxisColor;
    property AxisPosition: Single read FAxisPosition write FAxisPosition;
    property Border: TColor read FBorder write FBorder;
    property Color1: TColor read FColor1 write FColor1;
    property Color2: TColor read FColor2 write FColor2;
    property Position: Single read FPosition write FPosition;
  end;

  { TdxSpreadSheetCellDisplayStyle }

  TdxSpreadSheetCellDisplayStyle = class(TdxSpreadSheetCellStyle)
  strict private
    FDataBar: TdxSpreadSheetCellDataBar;
    FIconIndex: Integer;
    FShowCellValue: Boolean;
  protected
    function CreateDataBar: TdxSpreadSheetCellDataBar; virtual;
  public
    constructor Create(const AOwner: IdxSpreadSheetCellStyleOwner; AHandle: TdxSpreadSheetCellStyleHandle); override;
    destructor Destroy; override;
    procedure Reset;
    //
    property DataBar: TdxSpreadSheetCellDataBar read FDataBar;
    property IconIndex: Integer read FIconIndex write FIconIndex;
    property ShowCellValue: Boolean read FShowCellValue write FShowCellValue;
  end;

  { TdxSpreadSheetCellStyleMergeHelper }

  TdxSpreadSheetCellStyleMergeHelper = class
  strict private
    FIsUpdateBestFitNeeded: Boolean;
    FNewStyle: TdxSpreadSheetCellStyleHandle;
    FPrevStyle: TdxSpreadSheetCellStyleHandle;

    procedure MergeAlignment(ACellStyle: TdxSpreadSheetCellStyle);
    procedure MergeBorders(ACellStyle: TdxSpreadSheetCellStyle);
    procedure MergeBrush(ACellStyle: TdxSpreadSheetCellStyle);
    procedure MergeFont(ACellStyle: TdxSpreadSheetCellStyle);
    procedure MergeStates(ACellStyle: TdxSpreadSheetCellStyle);
  protected
    procedure CheckIsUpdateBestFitNeeded;

    property NewStyle: TdxSpreadSheetCellStyleHandle read FNewStyle;
    property PrevStyle: TdxSpreadSheetCellStyleHandle read FPrevStyle;
  public
    constructor Create(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
    procedure ProcessCellStyle(ACellStyle: TdxSpreadSheetCellStyle);
    //
    property IsUpdateBestFitNeeded: Boolean read FIsUpdateBestFitNeeded;
  end;

implementation

uses
  dxSpreadSheetUtils;

{ TdxSpreadSheetCellBorder }

constructor TdxSpreadSheetCellBorder.Create(AOwner: TdxSpreadSheetCellStyle; AKind: TcxBorder);
begin
  inherited Create;
  FOwner := AOwner;
  FKind := AKind;
end;

procedure TdxSpreadSheetCellBorder.Assign(ASource: TdxSpreadSheetCellBorder);
begin
  ChangeBorder(ASource.Style, ASource.Color);
end;

procedure TdxSpreadSheetCellBorder.Assign(ASource: TdxSpreadSheetBordersHandle);
begin
  ChangeBorder(ASource.BorderStyle[FKind], ASource.BorderColor[FKind]);
end;

procedure TdxSpreadSheetCellBorder.ChangeBorder(AStyle: TdxSpreadSheetCellBorderStyle; AColor: TColor);
var
  AHandle: TdxSpreadSheetBordersHandle;
begin
  if (Style <> AStyle) or (Color <> AColor) then
  begin
    AHandle := Handle.Clone;
    AHandle.BorderColor[Kind] := AColor;
    AHandle.BorderStyle[Kind] := AStyle;
    Handle := AHandle;
  end;
end;

function TdxSpreadSheetCellBorder.GetColor: TColor;
begin
  Result := Handle.BorderColor[Kind];
end;

function TdxSpreadSheetCellBorder.GetHandle: TdxSpreadSheetBordersHandle;
begin
  Result := Owner.Handle.Borders;
end;

function TdxSpreadSheetCellBorder.GetStyle: TdxSpreadSheetCellBorderStyle;
begin
  Result := Handle.BorderStyle[Kind];
end;

procedure TdxSpreadSheetCellBorder.Reset;
begin
  ChangeBorder(sscbsDefault, clDefault);
end;

procedure TdxSpreadSheetCellBorder.SetColor(const AValue: TColor);
begin
  ChangeBorder(Style, AValue);
end;

procedure TdxSpreadSheetCellBorder.SetHandle(const AValue: TdxSpreadSheetBordersHandle);
begin
  Owner.BeginUpdate;
  try
    Owner.Handle.Borders := Owner.CellStyles.Borders.AddBorders(AValue);
  finally
    Owner.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellBorder.SetStyle(const AValue: TdxSpreadSheetCellBorderStyle);
begin
  ChangeBorder(AValue, Color);
end;

{ TdxSpreadSheetCellBrush }

constructor TdxSpreadSheetCellBrush.Create(AOwner: TdxSpreadSheetCellStyle);
begin
  inherited Create(AOwner.CellStyles.Brushes);
  FOwner := AOwner;
end;

function TdxSpreadSheetCellBrush.GetHandle: TdxSpreadSheetBrushHandle;
begin
  Result := Owner.Handle.Brush;
end;

procedure TdxSpreadSheetCellBrush.SetHandle(const AValue: TdxSpreadSheetBrushHandle);
begin
  if AValue <> Handle then
  begin
    AValue.AddRef;
    try
      Owner.BeginUpdate;
      try
        Owner.Handle.Brush := AValue;
      finally
        Owner.EndUpdate;
      end;
    finally
      AValue.Release;
    end;
  end;
end;

{ TdxSpreadSheetCellDataFormat }

constructor TdxSpreadSheetCellDataFormat.Create(AOwner: TdxSpreadSheetCellStyle);
begin
  inherited Create(AOwner.CellStyles.Formats);
  FOwner := AOwner;
end;

procedure TdxSpreadSheetCellDataFormat.Format(const AValue: Variant;
  AValueType: TdxSpreadSheetCellDataType; var AResult: TdxSpreadSheetNumberFormatResult);
begin
  Handle.Format(AValue, AValueType, Owner.FormatSettings, AResult);
end;

function TdxSpreadSheetCellDataFormat.GetHandle: TdxSpreadSheetFormatHandle;
begin
  Result := Owner.Handle.DataFormat;
end;

procedure TdxSpreadSheetCellDataFormat.SetHandle(AValue: TdxSpreadSheetFormatHandle);
begin
  if AValue <> Handle then
  begin
    AValue.AddRef;
    try
      Owner.BeginUpdate;
      try
        Owner.Handle.DataFormat := AValue;
      finally
        Owner.EndUpdate;
      end;
    finally
      AValue.Release;
    end;
  end;
end;

function TdxSpreadSheetCellDataFormat.GetIsDateTime: Boolean;
begin
  Result := Handle.IsDateTime;
end;

function TdxSpreadSheetCellDataFormat.GetIsText: Boolean;
begin
  Result := Handle.IsText;
end;

function TdxSpreadSheetCellDataFormat.GetIsTime: Boolean;
begin
  Result := Handle.IsTime;
end;

{ TdxSpreadSheetCellFont }

constructor TdxSpreadSheetCellFont.Create(AOwner: TdxSpreadSheetCellStyle);
begin
  inherited Create(AOwner.CellStyles.Fonts);
  FOwner := AOwner;
end;

function TdxSpreadSheetCellFont.GetHandle: TdxSpreadSheetFontHandle;
begin
  Result := FOwner.Handle.Font;
end;

procedure TdxSpreadSheetCellFont.SetHandle(const AValue: TdxSpreadSheetFontHandle);
begin
  if AValue <> Handle then
  begin
    AValue.AddRef;
    try
      Owner.BeginUpdate;
      try
        Owner.Handle.Font := AValue;
      finally
        Owner.EndUpdate;
      end;
    finally
      AValue.Release;
    end;
  end;
end;

{ TdxSpreadSheetCellStyle }

constructor TdxSpreadSheetCellStyle.Create(const AOwner: IdxSpreadSheetCellStyleOwner);
begin
  Create(AOwner, AOwner.GetCellStyles.DefaultStyle);
end;

constructor TdxSpreadSheetCellStyle.Create(const AOwner: IdxSpreadSheetCellStyleOwner; AHandle: TdxSpreadSheetCellStyleHandle);
begin
  inherited Create;
  FOwner := AOwner;
  Inc(FUpdateLockCount);
  Handle := AHandle;
  Dec(FUpdateLockCount);
end;

destructor TdxSpreadSheetCellStyle.Destroy;
var
  ABorder: TcxBorder;
begin
  ReleaseHandle;
  FreeAndNil(FBrush);
  FreeAndNil(FFont);
  FreeAndNil(FDataFormat);
  for ABorder := Low(ABorder) to High(ABorder) do
    FreeAndNil(FBorders[ABorder]);
  inherited Destroy;
end;

procedure TdxSpreadSheetCellStyle.Assign(AStyle: TdxSpreadSheetCellStyle);
var
  ABorder: TcxBorder;
begin
  if Handle <> AStyle.Handle then
  begin
    BeginUpdate;
    try
      AlignHorz := AStyle.AlignHorz;
      AlignHorzIndent := AStyle.AlignHorzIndent;
      AlignVert := AStyle.AlignVert;
      Brush := AStyle.Brush;
      DataFormat := AStyle.DataFormat;
      Font := AStyle.Font;
      State := AStyle.State;
      for ABorder := Low(ABorder) to High(ABorder) do
        Borders[ABorder] := AStyle.Borders[ABorder];
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyle.BeginUpdate;
begin
  Inc(FUpdateLockCount);
  if UpdateLockCount = 1 then
    CloneHandle;
end;

procedure TdxSpreadSheetCellStyle.EndUpdate;
begin
  Dec(FUpdateLockCount);
  if UpdateLockCount = 0 then
    ReplaceHandle;
end;

procedure TdxSpreadSheetCellStyle.Merge(AColumnStyle, ARowStyle: TdxSpreadSheetCellStyle);
var
  ACellSourceStyle: TdxSpreadSheetCellStyle;
begin
  if ARowStyle.IsDefault then
    ACellSourceStyle := AColumnStyle
  else
    ACellSourceStyle := ARowStyle;

  if ARowStyle.Handle <> AColumnStyle.Handle then
  begin
    BeginUpdate;
    try
      Assign(ACellSourceStyle);
      Borders[bTop] := ARowStyle.Borders[bTop];
      Borders[bBottom] := ARowStyle.Borders[bBottom];
      Borders[bLeft] := AColumnStyle.Borders[bLeft];
      Borders[bRight] := AColumnStyle.Borders[bRight];
    finally
      EndUpdate;
    end;
  end
  else
    if UpdateLockCount = 0 then
      Handle := ACellSourceStyle.Handle
    else
      Assign(ACellSourceStyle);
end;

function TdxSpreadSheetCellStyle.GetState: TdxSpreadSheetCellStates;
begin
  Result := Handle.States;
end;

procedure TdxSpreadSheetCellStyle.SetState(const AValue: TdxSpreadSheetCellStates);
begin
  if State <> AValue then
  begin
    BeginUpdate;
    try
      Handle.States := AValue;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyle.Changed;
begin
  if UpdateLockCount = 0 then
    DoChanged;
end;

procedure TdxSpreadSheetCellStyle.CloneHandle;
begin
  FPrevHandle := Handle;
  FPrevHandle.AddRef;
  Handle := Handle.Clone;
end;

procedure TdxSpreadSheetCellStyle.DoChanged;
begin
  Owner.CellStyleChanged;
end;

function TdxSpreadSheetCellStyle.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := Owner.GetCellStyles;
end;

procedure TdxSpreadSheetCellStyle.SetHandle(const AHandle: TdxSpreadSheetCellStyleHandle);
begin
  if (FHandle <> nil) and (FHandle <> AHandle) and (AHandle <> nil) then
    Owner.CellStyleChanging;
  if dxChangeHandle(TdxHashTableItem(FHandle), AHandle) then
    Changed;
end;

procedure TdxSpreadSheetCellStyle.ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
begin
  Owner.ProcessStyleChanges(APrevStyle, ANewStyle);
end;

procedure TdxSpreadSheetCellStyle.ReleaseHandle;
begin
  Handle := nil;
end;

procedure TdxSpreadSheetCellStyle.ReplaceHandle;
begin
  if FPrevHandle.IsEqual(Handle) then
    Handle := FPrevHandle
  else
  begin
    Handle := CellStyles.AddStyle(Handle.Clone);
    ProcessStyleChanges(FPrevHandle, Handle);
  end;
  FPrevHandle.Release;
  FPrevHandle := nil;
end;

procedure TdxSpreadSheetCellStyle.ChangeState(AState: TdxSpreadSheetCellState; AValue: Boolean);
begin
  if AValue then
    State := State + [AState]
  else
    State := State - [AState];
end;

function TdxSpreadSheetCellStyle.GetAlignHorz: TdxSpreadSheetDataAlignHorz;
begin
  Result := Handle.AlignHorz;
end;

function TdxSpreadSheetCellStyle.GetAlignHorzIndent: Integer;
begin
  Result := Handle.AlignHorzIndent;
end;

function TdxSpreadSheetCellStyle.GetAlignVert: TdxSpreadSheetDataAlignVert;
begin
  Result := Handle.AlignVert;
end;

function TdxSpreadSheetCellStyle.GetBorder(ABorder: TcxBorder): TdxSpreadSheetCellBorder;
begin
  if FBorders[ABorder] = nil then
    FBorders[ABorder] := TdxSpreadSheetCellBorder.Create(Self, ABorder);
  Result := FBorders[ABorder];
end;

function TdxSpreadSheetCellStyle.GetBrush: TdxSpreadSheetCellBrush;
begin
  if FBrush = nil then
    FBrush := TdxSpreadSheetCellBrush.Create(Self);
  Result := FBrush;
end;

function TdxSpreadSheetCellStyle.GetHidden: Boolean;
begin
  Result := csHidden in Handle.States;
end;

function TdxSpreadSheetCellStyle.GetDataFormat: TdxSpreadSheetCellDataFormat;
begin
  if FDataFormat = nil then
    FDataFormat := TdxSpreadSheetCellDataFormat.Create(Self);
  Result := FDataFormat;
end;

function TdxSpreadSheetCellStyle.GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  Result := Owner.FormatSettings;
end;

function TdxSpreadSheetCellStyle.GetIsDefault: Boolean;
begin
  Result := Handle.IsDefault;
end;

function TdxSpreadSheetCellStyle.GetFont: TdxSpreadSheetCellFont;
begin
  if FFont = nil then
    FFont := TdxSpreadSheetCellFont.Create(Self);
  Result := FFont;
end;

function TdxSpreadSheetCellStyle.GetLocked: Boolean;
begin
  Result := csLocked in Handle.States;
end;

function TdxSpreadSheetCellStyle.GetShrinkToFit: Boolean;
begin
  Result := csShrinkToFit in Handle.States;
end;

function TdxSpreadSheetCellStyle.GetWordWrap: Boolean;
begin
  Result := csWordWrap in Handle.States;
end;

procedure TdxSpreadSheetCellStyle.SetAlignHorz(const AValue: TdxSpreadSheetDataAlignHorz);
begin
  if AlignHorz <> AValue then
  begin
    BeginUpdate;
    try
      Handle.AlignHorz := AValue;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyle.SetAlignHorzIndent(const AValue: Integer);
begin
  if AlignHorzIndent <> AValue then
  begin
    BeginUpdate;
    try
      Handle.AlignHorzIndent := AValue;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyle.SetAlignVert(const AValue: TdxSpreadSheetDataAlignVert);
begin
  if AlignVert <> AValue then
  begin
    BeginUpdate;
    try
      Handle.AlignVert := AValue;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetCellStyle.SetBorder(ABorder: TcxBorder; const AValue: TdxSpreadSheetCellBorder);
begin
  Borders[ABorder].Assign(AValue);
end;

procedure TdxSpreadSheetCellStyle.SetBrush(const AValue: TdxSpreadSheetCellBrush);
begin
  Brush.Assign(AValue);
end;

procedure TdxSpreadSheetCellStyle.SetDataFormat(const AValue: TdxSpreadSheetCellDataFormat);
begin
  DataFormat.Assign(AValue);
end;

procedure TdxSpreadSheetCellStyle.SetFont(const AValue: TdxSpreadSheetCellFont);
begin
  Font.Assign(AValue);
end;

procedure TdxSpreadSheetCellStyle.SetHidden(const AValue: Boolean);
begin
  ChangeState(csHidden, AValue);
end;

procedure TdxSpreadSheetCellStyle.SetIsDefault(const Value: Boolean);
begin
  if Value then
    Handle := CellStyles.DefaultStyle;
end;

procedure TdxSpreadSheetCellStyle.SetLocked(const AValue: Boolean);
begin
  ChangeState(csLocked, AValue);
end;

procedure TdxSpreadSheetCellStyle.SetShrinkToFit(const AValue: Boolean);
begin
  ChangeState(csShrinkToFit, AValue);
end;

procedure TdxSpreadSheetCellStyle.SetWordWrap(const AValue: Boolean);
begin
  ChangeState(csWordWrap, AValue);
end;

{ TdxSpreadSheetCellDataBar }

procedure TdxSpreadSheetCellDataBar.AfterConstruction;
begin
  inherited AfterConstruction;
  Reset;
end;

procedure TdxSpreadSheetCellDataBar.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetCellDataBar then
  begin
    AxisColor := TdxSpreadSheetCellDataBar(Source).AxisColor;
    AxisPosition := TdxSpreadSheetCellDataBar(Source).AxisPosition;
    Border := TdxSpreadSheetCellDataBar(Source).Border;
    Color1 := TdxSpreadSheetCellDataBar(Source).Color1;
    Color2 := TdxSpreadSheetCellDataBar(Source).Color2;
    Position := TdxSpreadSheetCellDataBar(Source).Position;
  end;
end;

function TdxSpreadSheetCellDataBar.IsEmpty: Boolean;
begin
  Result := Color1 = clNone;
end;

procedure TdxSpreadSheetCellDataBar.Reset;
begin
  FAxisColor := clNone;
  FAxisPosition := 0;
  FPosition := 0;
  FBorder := clNone;
  FColor1 := clNone;
  FColor2 := clNone;
end;

procedure TdxSpreadSheetCellDataBar.ReadData(AReader: TReader);
begin
  if AReader.ReadInteger >= 1 then
  begin
    FAxisColor := AReader.ReadInteger;
    FAxisPosition := AReader.ReadSingle;
    FBorder := AReader.ReadInteger;
    FColor1 := AReader.ReadInteger;
    FColor2 := AReader.ReadInteger;
    FPosition := AReader.ReadSingle;
  end;
end;

procedure TdxSpreadSheetCellDataBar.WriteData(AWriter: TWriter);
begin
  AWriter.WriteInteger(1);
  AWriter.WriteInteger(AxisColor);
  AWriter.WriteSingle(AxisPosition);
  AWriter.WriteInteger(Border);
  AWriter.WriteInteger(Color1);
  AWriter.WriteInteger(Color2);
  AWriter.WriteSingle(Position);
end;

{ TdxSpreadSheetCellDisplayStyle }

constructor TdxSpreadSheetCellDisplayStyle.Create(
  const AOwner: IdxSpreadSheetCellStyleOwner; AHandle: TdxSpreadSheetCellStyleHandle);
begin
  inherited Create(AOwner, AHandle);
  FShowCellValue := True;
  FDataBar := CreateDataBar;
  FIconIndex := -1;
end;

destructor TdxSpreadSheetCellDisplayStyle.Destroy;
begin
  FreeAndNil(FDataBar);
  inherited Destroy;
end;

procedure TdxSpreadSheetCellDisplayStyle.Reset;
begin
  IconIndex := -1;
  ShowCellValue := True;
  DataBar.Reset;
end;

function TdxSpreadSheetCellDisplayStyle.CreateDataBar: TdxSpreadSheetCellDataBar;
begin
  Result := TdxSpreadSheetCellDataBar.Create;
end;

{ TdxSpreadSheetCellStyleMergeHelper }

constructor TdxSpreadSheetCellStyleMergeHelper.Create(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
begin
  inherited Create;
  FNewStyle := ANewStyle;
  FPrevStyle := APrevStyle;
  CheckIsUpdateBestFitNeeded;
end;

procedure TdxSpreadSheetCellStyleMergeHelper.ProcessCellStyle(ACellStyle: TdxSpreadSheetCellStyle);
begin
  ACellStyle.BeginUpdate;
  try
    MergeAlignment(ACellStyle);
    MergeBrush(ACellStyle);
    MergeFont(ACellStyle);
    MergeBorders(ACellStyle);
    MergeStates(ACellStyle);

    if PrevStyle.DataFormat <> NewStyle.DataFormat then
      ACellStyle.Handle.DataFormat := NewStyle.DataFormat;
  finally
    ACellStyle.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleMergeHelper.CheckIsUpdateBestFitNeeded;
begin
  FIsUpdateBestFitNeeded := (PrevStyle.DataFormat <> NewStyle.DataFormat) or
    (PrevStyle.Font <> NewStyle.Font) or (PrevStyle.States <> NewStyle.States) or
    (PrevStyle.Borders <> NewStyle.Borders);
end;

procedure TdxSpreadSheetCellStyleMergeHelper.MergeAlignment(ACellStyle: TdxSpreadSheetCellStyle);
begin
  if PrevStyle.AlignHorz <> NewStyle.AlignHorz then
    ACellStyle.AlignHorz := NewStyle.AlignHorz;
  if PrevStyle.AlignHorzIndent <> NewStyle.AlignHorzIndent then
    ACellStyle.AlignHorzIndent := NewStyle.AlignHorzIndent;
  if PrevStyle.AlignVert <> NewStyle.AlignVert then
    ACellStyle.AlignVert := NewStyle.AlignVert;
end;

procedure TdxSpreadSheetCellStyleMergeHelper.MergeBorders(ACellStyle: TdxSpreadSheetCellStyle);
var
  ABorder: TcxBorder;
begin
  for ABorder := Low(ABorder) to High(ABorder) do
  begin
    if PrevStyle.Borders.BorderColor[ABorder] <> NewStyle.Borders.BorderColor[ABorder] then
      ACellStyle.Borders[ABorder].Color := NewStyle.Borders.BorderColor[ABorder];
    if PrevStyle.Borders.BorderStyle[ABorder] <> NewStyle.Borders.BorderStyle[ABorder] then
      ACellStyle.Borders[ABorder].Style := NewStyle.Borders.BorderStyle[ABorder];
  end;
end;

procedure TdxSpreadSheetCellStyleMergeHelper.MergeBrush(ACellStyle: TdxSpreadSheetCellStyle);
begin
  if PrevStyle.Brush.BackgroundColor <> NewStyle.Brush.BackgroundColor then
    ACellStyle.Brush.BackgroundColor := NewStyle.Brush.BackgroundColor;
  if PrevStyle.Brush.ForegroundColor <> NewStyle.Brush.ForegroundColor then
    ACellStyle.Brush.ForegroundColor := NewStyle.Brush.ForegroundColor;
  if PrevStyle.Brush.Style <> NewStyle.Brush.Style then
    ACellStyle.Brush.Style := NewStyle.Brush.Style;
end;

procedure TdxSpreadSheetCellStyleMergeHelper.MergeFont(ACellStyle: TdxSpreadSheetCellStyle);

  procedure CheckFontStyle(AStyle: TFontStyle);
  begin
    if (AStyle in PrevStyle.Font.Style) <> (AStyle in NewStyle.Font.Style) then
    begin
      if AStyle in NewStyle.Font.Style then
        ACellStyle.Font.Style := ACellStyle.Font.Style + [AStyle]
      else
        ACellStyle.Font.Style := ACellStyle.Font.Style - [AStyle];
    end;
  end;

var
  AStyle: TFontStyle;
begin
  if PrevStyle.Font.Charset <> NewStyle.Font.Charset then
    ACellStyle.Font.Charset := NewStyle.Font.Charset;
  if PrevStyle.Font.Color <> NewStyle.Font.Color then
    ACellStyle.Font.Color := NewStyle.Font.Color;
  if PrevStyle.Font.Size <> NewStyle.Font.Size then
    ACellStyle.Font.Size := NewStyle.Font.Size;
  if PrevStyle.Font.Name <> NewStyle.Font.Name then
    ACellStyle.Font.Name := NewStyle.Font.Name;
  if PrevStyle.Font.Pitch <> NewStyle.Font.Pitch then
    ACellStyle.Font.Pitch := NewStyle.Font.Pitch;
  if PrevStyle.Font.Style <> NewStyle.Font.Style then
  begin
    for AStyle := Low(AStyle) to High(AStyle) do
      CheckFontStyle(AStyle);
  end;
end;

procedure TdxSpreadSheetCellStyleMergeHelper.MergeStates(ACellStyle: TdxSpreadSheetCellStyle);
var
  AState: TdxSpreadSheetCellState;
begin
  if PrevStyle.States <> NewStyle.States then
    for AState := Low(TdxSpreadSheetCellState) to High(TdxSpreadSheetCellState) do
      if (AState in PrevStyle.States) <> (AState in NewStyle.States) then
      begin
        if AState in NewStyle.States then
          ACellStyle.Handle.States := ACellStyle.Handle.States + [AState]
        else
          ACellStyle.Handle.States := ACellStyle.Handle.States - [AState];
      end;
end;

end.
