{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.DocumentModel.PatternLine;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCore, Generics.Defaults, Generics.Collections, dxCoreClasses, Graphics, Types, cxGeometry, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.CharacterFormatting;

type
  TdxStrikeout = class;
  TdxUnderline = class;
  TdxStrikeoutSingle = class;
  TdxStrikeoutDouble = class;
  TdxUnderlineSingle = class;
  TdxUnderlineDouble = class;
  TdxUnderlineThickDashDotted = class;
  TdxUnderlineHeavyWave = class;
  TdxUnderlineDoubleWave = class;
  TdxUnderlineDotted = class;
  TdxUnderlineDashed = class;
  TdxUnderlineDashSmallGap = class;
  TdxUnderlineDashDotted = class;
  TdxUnderlineDashDotDotted = class;
  TdxUnderlineLongDashed = class;
  TdxUnderlineThickSingle = class;
  TdxUnderlineThickDotted = class;
  TdxUnderlineThickDashed = class;
  TdxUnderlineThickDashDotDotted = class;
  TdxUnderlineThickLongDashed = class;
  TdxUnderlineWave = class;

  IdxPatternLinePainter = interface
  ['{01E8601D-8092-456F-9588-7D4BA1DD538F}']
  end;

  IdxStrikeoutPainter = interface(IdxPatternLinePainter)
  ['{66C8FFB2-B0F7-40D1-81E7-E1C47B38C6DA}']
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
  end;

  IdxUnderlinePainter = interface(IdxPatternLinePainter)
  ['{40939A5B-DEE6-4757-9687-6C061D26B513}']
    procedure DrawUnderline(AUnderline: TdxUnderlineSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashSmallGap; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineHeavyWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDoubleWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
  end;

  { IdxCharacterLinePainter }

  IdxCharacterLinePainter = interface(IdxUnderlinePainter)
  ['{64F8272A-25B3-4257-84D7-1FD7596A893D}']
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
  end;

  { IdxPainterWrapper }

  IdxPainterWrapper = interface
  ['{6AC1FDF4-EB48-4CB1-9044-D4F4A7D38DDA}']
    function GetHorizontalLinePainter: IdxCharacterLinePainter;
    function GetVerticalLinePainter: IdxCharacterLinePainter;

    procedure SnapWidths(var AWidths: TArray<Single>);
    procedure SnapHeights(var AHeights: TArray<Single>);
    procedure FillRectangle(AColor: TdxAlphaColor; const ABounds: TdxRectF);
    function GetSnappedPoint(const APoint: TdxPointF): TdxPointF;

    property HorizontalLinePainter: IdxCharacterLinePainter read GetHorizontalLinePainter;
    property VerticalLinePainter: IdxCharacterLinePainter read GetVerticalLinePainter;
  end;

  { TdxPatternLine }

  TdxPatternLine<T> = class
  protected
    function GetId: T; virtual; abstract;
  public
    function CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect; virtual; abstract;
    procedure Draw(const APainter: IdxPatternLinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); virtual; abstract;
    function CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single; virtual;

    property Id: T read GetId;
  end;

  { TdxPatternLineRepository }

  TdxPatternLineRepository = class abstract
  protected
    FCollection: TdxFastObjectList;
    procedure PopulateRepository; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterPatternLine(ALine: TObject): Boolean;
    function UnregisterPatternLine(ALine: TObject): Boolean; overload;
    function UnregisterPatternLine(AType: TClass): Boolean; overload;
    function GetPatternLineByTypeInternal(AType: TClass): TObject;
    function GetPatternLineByType(ALine: TClass): Pointer;

    property Items: TdxFastObjectList read FCollection;
  end;

  { TdxStrikeout }

  TdxStrikeout = class abstract (TdxPatternLine<TdxStrikeoutType>)
  public
    procedure Draw(const APainter: IdxPatternLinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); reintroduce; overload; override;
    procedure Draw(const APainter: IdxStrikeoutPainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); reintroduce; overload; virtual; abstract;
  end;

  { TdxStrikeoutSingle }

  TdxStrikeoutSingle = class(TdxStrikeout)
  protected
    function GetId: TdxStrikeoutType; override;
  public
    procedure Draw(const APainter: IdxStrikeoutPainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
    function CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect; override;
    function CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single; override;
  end;

  { TdxStrikeoutDouble }

  TdxStrikeoutDouble = class(TdxStrikeout)
  protected
    function GetId: TdxStrikeoutType; override;
  public
    procedure Draw(const APainter: IdxStrikeoutPainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
    function CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect; override;
    function CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single; override;
  end;

  { TdxStrikeoutRepository }

  TdxStrikeoutRepository = class(TdxPatternLineRepository)
  protected
    procedure PopulateRepository; override;
  public
    function GetPatternLineByType(ALine: TdxStrikeoutType): Pointer;
  end;

  { TdxUnderline }

  TdxUnderline = class(TdxPatternLine<TdxUnderlineType>)
  strict private
    class var FUnderlineNone: TdxUnderline;
    class function GetUnderlineNone: TdxUnderline; static;
  protected
    class destructor Finalize;
  public
    procedure Draw(const APainter: IdxPatternLinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload; override;
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); reintroduce; overload; virtual; abstract;

    class property UnderlineNone: TdxUnderline read GetUnderlineNone;
  end;
  TdxUnderlineClass = class of TdxUnderline;

  { TdxNoneUnderline }

  TdxNoneUnderline = class(TdxUnderline)
  protected
    function GetId: TdxUnderlineType; override;
  public
    function CalcLineBounds(const R: TRect; AThickness: Integer): TRect; override;
    procedure Draw(const APainter: IdxPatternLinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineThinSize }

  TdxUnderlineThinSize = class abstract (TdxUnderline)
  public
    function CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect; override;
  end;

  { TdxUnderlineFullSize }

  TdxUnderlineFullSize = class abstract (TdxUnderline)
  public
    function CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect; override;
  end;

  { TdxUnderlineThickSize }

  TdxUnderlineThickSize = class abstract (TdxUnderline)
  public
    function CalcLineBounds(const R: TRect; AThickness: Integer): TRect; override;
  end;

  { TdxUnderlineNone }

  TdxUnderlineNone = class(TdxUnderline)
  protected
    function GetId: TdxUnderlineType; override;
  public
    function CalcLineBounds(const R: TRect; AThickness: Integer): TRect; override;
    procedure Draw(const APainter: IdxPatternLinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineSingle }

  TdxUnderlineSingle = class(TdxUnderlineThinSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineDotted }

  TdxUnderlineDotted = class(TdxUnderlineThinSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineDashed }

  TdxUnderlineDashed = class(TdxUnderlineThinSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineDashSmallGap }

  TdxUnderlineDashSmallGap = class(TdxUnderlineThinSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineDashDotted }

  TdxUnderlineDashDotted = class(TdxUnderlineThinSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineDashDotDotted }

  TdxUnderlineDashDotDotted = class(TdxUnderlineThinSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineDouble }

  TdxUnderlineDouble = class(TdxUnderlineFullSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    function CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single; override;
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineHeavyWave }

  TdxUnderlineHeavyWave = class(TdxUnderlineFullSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    function CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single; override;
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineLongDashed }

  TdxUnderlineLongDashed = class(TdxUnderlineThinSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineThickSingle }

  TdxUnderlineThickSingle = class(TdxUnderlineThickSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineThickDotted }

  TdxUnderlineThickDotted = class(TdxUnderlineThickSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineThickDashed }

  TdxUnderlineThickDashed = class(TdxUnderlineThickSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineThickDashDotted }

  TdxUnderlineThickDashDotted = class(TdxUnderlineThickSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineThickDashDotDotted }

  TdxUnderlineThickDashDotDotted = class(TdxUnderlineThickSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineThickLongDashed }

  TdxUnderlineThickLongDashed = class(TdxUnderlineThickSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineDoubleWave }

  TdxUnderlineDoubleWave = class(TdxUnderlineFullSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    function CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single; override;
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineWave }

  TdxUnderlineWave = class(TdxUnderlineFullSize)
  protected
    function GetId: TdxUnderlineType; override;
  public
    function CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single; override;
    procedure Draw(const APainter: IdxUnderlinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor); override;
  end;

  { TdxUnderlineRepository }

  TdxUnderlineRepository = class(TdxPatternLineRepository)
  protected
    procedure PopulateRepository; override;
  public
    function GetPatternLineByType(ALine: TdxUnderlineType): Pointer;
  end;

implementation

uses
  RTLConsts, Classes, SysUtils, dxTypeHelpers;

{ TdxPatternLine }

function TdxPatternLine<T>.CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single;
begin
  Result := ALineBounds.Height / 2;
end;

{ TdxStrikeout }

procedure TdxStrikeout.Draw(const APainter: IdxPatternLinePainter; const ABounds: TdxRectF; AColor: TdxAlphaColor);
var
  AStrikeoutPainter: IdxStrikeoutPainter;
begin
  if Supports(APainter, IdxStrikeoutPainter, AStrikeoutPainter) then
    Draw(AStrikeoutPainter, ABounds, AColor);
end;

{ TdxPatternLineRepository }

constructor TdxPatternLineRepository.Create;
begin
  inherited Create;
  FCollection := TdxFastObjectList.Create;
  PopulateRepository;
end;

destructor TdxPatternLineRepository.Destroy;
begin
  FreeAndNil(FCollection);
  inherited Destroy;
end;

function TdxPatternLineRepository.GetPatternLineByType(ALine: TClass): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCollection.Count - 1 do
    if FCollection[I].ClassType = ALine then
    begin
      Result := FCollection[I];
      Break;
    end;
end;

function TdxPatternLineRepository.GetPatternLineByTypeInternal(AType: TClass): TObject;
begin
  Result := GetPatternLineByType(AType);
  if Result = nil then
    Result := FCollection[0];
end;

function TdxPatternLineRepository.RegisterPatternLine(ALine: TObject): Boolean;
var
  AExistingPatternLine: TObject;
begin
  Result := False;
  Assert(ALine <> nil, 'ALine = nil');

  AExistingPatternLine := GetPatternLineByType(ALine.ClassType);
  if AExistingPatternLine = nil then
  begin
    FCollection.Add(ALine);
    Result := True;
  end;
end;

function TdxPatternLineRepository.UnregisterPatternLine(AType: TClass): Boolean;
var
  ALine: TObject;
begin
  Result := False;
  ALine := GetPatternLineByType(AType);
  if ALine = nil then
  begin
    FCollection.Remove(ALine);
    Result := True;
  end;
end;

function TdxPatternLineRepository.UnregisterPatternLine(ALine: TObject): Boolean;
begin
  Assert(ALine <> nil, 'ALine = nil');
  Result := UnregisterPatternLine(ALine.ClassType);
end;

{ TdxStrikeoutRepository }

function TdxStrikeoutRepository.GetPatternLineByType(ALine: TdxStrikeoutType): Pointer;
var
  AType: TClass;
begin
  case ALine of
    TdxStrikeoutType.Single:
      AType := TdxStrikeoutSingle;
    TdxStrikeoutType.Double:
      AType := TdxStrikeoutDouble;
    else
      raise Exception.Create('Error StrikeoutType');
  end;
  Result := inherited GetPatternLineByType(AType);
end;

procedure TdxStrikeoutRepository.PopulateRepository;
begin
  RegisterPatternLine(TdxStrikeoutSingle.Create);
  RegisterPatternLine(TdxStrikeoutDouble.Create);
end;

{ TdxUnderline }

class destructor TdxUnderline.Finalize;
begin
  FUnderlineNone.Free;
end;

class function TdxUnderline.GetUnderlineNone: TdxUnderline;
begin
  if FUnderlineNone = nil then
    FUnderlineNone := TdxNoneUnderline.Create;
  Result := FUnderlineNone;
end;

procedure TdxUnderline.Draw(const APainter: IdxPatternLinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
var
  AUnderlinePainter: IdxUnderlinePainter;
begin
  if Supports(APainter, IdxUnderlinePainter, AUnderlinePainter) then
    Draw(AUnderlinePainter, ABounds, AColor);
end;

{ TdxNoneUnderline }

function TdxNoneUnderline.CalcLineBounds(const R: TRect; AThickness: Integer): TRect;
begin
  Result.Empty;
end;

procedure TdxNoneUnderline.Draw(const APainter: IdxPatternLinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
end;

procedure TdxNoneUnderline.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
end;

function TdxNoneUnderline.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.None;
end;

{ TdxStrikeoutSingle }

function TdxStrikeoutSingle.CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect;
begin
  Result.InitSize(ARect.Left, ARect.Top, ARect.Width, AThickness);
end;

function TdxStrikeoutSingle.CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single;
begin
  Result := ALineBounds.Height / 2;
end;

procedure TdxStrikeoutSingle.Draw(const APainter: IdxStrikeoutPainter; const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawStrikeout(Self, ABounds, AColor);
end;

function TdxStrikeoutSingle.GetId: TdxStrikeoutType;
begin
  Result := TdxStrikeoutType.Single;
end;

{ TdxStrikeoutDouble }

function TdxStrikeoutDouble.CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect;
begin
  Result.InitSize(ARect.Left, ARect.Top - (3 * AThickness) div 2, ARect.Width, 3 * AThickness)
end;

function TdxStrikeoutDouble.CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single;
begin
  Result := 0;
end;

procedure TdxStrikeoutDouble.Draw(const APainter: IdxStrikeoutPainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawStrikeout(Self, ABounds, AColor);
end;

function TdxStrikeoutDouble.GetId: TdxStrikeoutType;
begin
  Result := TdxStrikeoutType.Double;
end;

{ TdxUnderlineThinSize }

function TdxUnderlineThinSize.CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect;
begin
  Result.InitSize(ARect.Left, ARect.Bottom - AThickness, cxRectWidth(ARect), AThickness);
end;

{ TdxUnderlineFullSize }

function TdxUnderlineFullSize.CalcLineBounds(const ARect: TRect; AThickness: Integer): TRect;
begin
  Result := ARect;
end;

{ TdxUnderlineThickSize }

function TdxUnderlineThickSize.CalcLineBounds(const R: TRect; AThickness: Integer): TRect;
begin
  Result.InitSize(R.Left, R.Top, R.Width, R.Height - AThickness div 2);
end;

{ TdxUnderlineNone }

function TdxUnderlineNone.CalcLineBounds(const R: TRect; AThickness: Integer): TRect;
begin
  Result := TRect.Null;
end;

procedure TdxUnderlineNone.Draw(const APainter: IdxPatternLinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
end;

procedure TdxUnderlineNone.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
end;

function TdxUnderlineNone.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.None;
end;

{ TdxUnderlineSingle }

procedure TdxUnderlineSingle.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineSingle.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Single;
end;

{ TdxUnderlineDotted }

procedure TdxUnderlineDotted.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineDotted.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Dotted;
end;

{ TdxUnderlineDashed }

procedure TdxUnderlineDashed.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineDashed.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Dashed;
end;

{ TdxUnderlineDashSmallGap }

procedure TdxUnderlineDashSmallGap.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineDashSmallGap.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Dashed;
end;

{ TdxUnderlineDashDotted }

procedure TdxUnderlineDashDotted.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineDashDotted.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.DashDotted;
end;

{ TdxUnderlineDashDotDotted }

procedure TdxUnderlineDashDotDotted.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineDashDotDotted.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.DashDotDotted;
end;

{ TdxUnderlineDouble }

function TdxUnderlineDouble.CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single;
begin
  Result := 0;
end;

procedure TdxUnderlineDouble.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineDouble.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Double;
end;

{ TdxUnderlineHeavyWave }

function TdxUnderlineHeavyWave.CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single;
begin
  Result := 0;
end;

procedure TdxUnderlineHeavyWave.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineHeavyWave.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.HeavyWave;
end;

{ TdxUnderlineLongDashed }

procedure TdxUnderlineLongDashed.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineLongDashed.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.LongDashed;
end;

{ TdxUnderlineThickSingle }

procedure TdxUnderlineThickSingle.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineThickSingle.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.ThickSingle;
end;

{ TdxUnderlineThickDotted }

procedure TdxUnderlineThickDotted.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineThickDotted.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.ThickDotted;
end;

{ TdxUnderlineThickDashed }

procedure TdxUnderlineThickDashed.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineThickDashed.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.ThickDashed;
end;

{ TdxUnderlineThickDashDotted }

procedure TdxUnderlineThickDashDotted.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineThickDashDotted.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.ThickDashDotted;
end;

{ TdxUnderlineThickDashDotDotted }

procedure TdxUnderlineThickDashDotDotted.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineThickDashDotDotted.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.ThickDashDotDotted;
end;

{ TdxUnderlineThickLongDashed }

procedure TdxUnderlineThickLongDashed.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineThickLongDashed.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.ThickLongDashed;
end;

{ TdxUnderlineDoubleWave }

function TdxUnderlineDoubleWave.CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single;
begin
  Result := 0;
end;

procedure TdxUnderlineDoubleWave.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineDoubleWave.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.DoubleWave;
end;

{ TdxUnderlineWave }

function TdxUnderlineWave.CalcLinePenVerticalOffset(const ALineBounds: TdxRectF): Single;
begin
  Result := 0;
end;

procedure TdxUnderlineWave.Draw(const APainter: IdxUnderlinePainter;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  APainter.DrawUnderline(Self, ABounds, AColor);
end;

function TdxUnderlineWave.GetId: TdxUnderlineType;
begin
  Result := TdxUnderlineType.Wave;
end;

{ TdxUnderlineRepository }

function TdxUnderlineRepository.GetPatternLineByType(ALine: TdxUnderlineType): Pointer;
var
  AType: TClass;
begin
  case ALine of
    TdxUnderlineType.None:
      AType := TdxUnderlineNone;
    TdxUnderlineType.Single:
      AType := TdxUnderlineSingle;
    TdxUnderlineType.Dotted:
      AType := TdxUnderlineDotted;
    TdxUnderlineType.Dashed:
      AType := TdxUnderlineDashed;
    TdxUnderlineType.DashDotted:
      AType := TdxUnderlineDashDotted;
    TdxUnderlineType.DashDotDotted:
      AType := TdxUnderlineDashDotDotted;
    TdxUnderlineType.Double:
      AType := TdxUnderlineDouble;
    TdxUnderlineType.HeavyWave:
      AType := TdxUnderlineHeavyWave;
    TdxUnderlineType.LongDashed:
      AType := TdxUnderlineLongDashed;
    TdxUnderlineType.ThickSingle:
      AType := TdxUnderlineThickSingle;
    TdxUnderlineType.ThickDotted:
      AType := TdxUnderlineThickDotted;
    TdxUnderlineType.ThickDashed:
      AType := TdxUnderlineThickDashed;
    TdxUnderlineType.ThickDashDotted:
      AType := TdxUnderlineThickDashDotted;
    TdxUnderlineType.ThickDashDotDotted:
      AType := TdxUnderlineThickDashDotDotted;
    TdxUnderlineType.ThickLongDashed:
      AType := TdxUnderlineThickLongDashed;
    TdxUnderlineType.DoubleWave:
      AType := TdxUnderlineDoubleWave;
    TdxUnderlineType.Wave:
      AType := TdxUnderlineWave;
    TdxUnderlineType.DashSmallGap:
      AType := TdxUnderlineDashSmallGap;
    else
      raise Exception.Create('Error StrikeoutType');
  end;
  Result := inherited GetPatternLineByType(AType);
end;

procedure TdxUnderlineRepository.PopulateRepository;
begin
  RegisterPatternLine(TdxUnderlineNone.Create);
  RegisterPatternLine(TdxUnderlineSingle.Create);
  RegisterPatternLine(TdxUnderlineDotted.Create);
  RegisterPatternLine(TdxUnderlineDashed.Create);
  RegisterPatternLine(TdxUnderlineDashDotted.Create);
  RegisterPatternLine(TdxUnderlineDashDotDotted.Create);
  RegisterPatternLine(TdxUnderlineDouble.Create);
  RegisterPatternLine(TdxUnderlineHeavyWave.Create);
  RegisterPatternLine(TdxUnderlineLongDashed.Create);
  RegisterPatternLine(TdxUnderlineThickSingle.Create);
  RegisterPatternLine(TdxUnderlineThickDotted.Create);
  RegisterPatternLine(TdxUnderlineThickDashed.Create);
  RegisterPatternLine(TdxUnderlineThickDashDotted.Create);
  RegisterPatternLine(TdxUnderlineThickDashDotDotted.Create);
  RegisterPatternLine(TdxUnderlineThickLongDashed.Create);
  RegisterPatternLine(TdxUnderlineDoubleWave.Create);
  RegisterPatternLine(TdxUnderlineWave.Create);
  RegisterPatternLine(TdxUnderlineDashSmallGap.Create);
end;

end.
