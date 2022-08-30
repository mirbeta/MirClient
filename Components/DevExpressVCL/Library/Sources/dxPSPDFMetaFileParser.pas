{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSPDFMetaFileParser;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, SysUtils, Graphics,
  dxCore, cxGeometry, cxClasses, cxGraphics, cxDrawTextUtils, dxBkGnd,
  dxPSCore, dxPSPDFExportCore, dxPSPDFExport,
  dxPSGlbl, dxPSFillPatterns, dxPSReportRenderCanvas;

type

  { TdxPSPDFMetaFileGdiObjectItem }

  TdxPSPDFMetaFileGdiObjectItem = class(TObject)
  private
    FColor: TColor;
    FHandle: THandle;
    FSourceObject: TObject;
  public
    constructor Create(AHandle: THandle; ASourceObject: TObject; AColor: TColor);
    destructor Destroy; override;
    //
    property Color: TColor read FColor;
    property Handle: THandle read FHandle;
    property SourceObject: TObject read FSourceObject;
  end;

  { TdxPSPDFMetaFileGdiObjectList }

  TdxPSPDFMetaFileGdiObjectList = class(TList)
  private
    function GetItem(Index: Integer): TdxPSPDFMetaFileGdiObjectItem;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function CreateBrush(const ABrushInfo: {$IFDEF DELPHI16}TLogBrush32{$ELSE}TLogBrush{$ENDIF}): HBRUSH;
    function CreateFont(const AFontInfo: TExtLogFontW): HFONT;
    function CreatePen(const APenInfo: TLogPen): HPEN;
    function CreatePenEx(const APenInfo: {$IFDEF DELPHI16}TExtLogPen32{$ELSE}TExtLogPen{$ENDIF}): HPEN;
    function FindObject(AHandle: THandle; var AObject: TdxPSPDFMetaFileGdiObjectItem): Boolean;
    //
    property Items[Index: Integer]: TdxPSPDFMetaFileGdiObjectItem read GetItem;
  end;

  { TdxPSPDFMetaFileCanvasState }

  TdxPSPDFMetaFileCanvasState = class(TObject)
  strict private
    FBackgroundColor: TColor;
    FBackgroundMode: Integer;
    FBrush: TBrush;
    FMapMode: Integer;
    FPen: TPen;
    FPolyFillMode: Integer;
    FTextColor: TColor;
    FTransform: TXForm;
    FViewPortExt: TSize;
    FViewPortOrg: TPoint;
    FWindowExt: TSize;
    FWindowOrg: TPoint;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TdxPSPDFMetaFileCanvasState);
    function Clone: TdxPSPDFMetaFileCanvasState;
    //
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property BackgroundMode: Integer read FBackgroundMode write FBackgroundMode;
    property Brush: TBrush read FBrush;
    property MapMode: Integer read FMapMode write FMapMode;
    property Pen: TPen read FPen;
    property PolyFillMode: Integer read FPolyFillMode write FPolyFillMode;
    property TextColor: TColor read FTextColor write FTextColor;
    property Transform: TXForm read FTransform write FTransform;
    property ViewPortExt: TSize read FViewPortExt write FViewPortExt;
    property ViewPortOrg: TPoint read FViewPortOrg write FViewPortOrg;
    property WindowExt: TSize read FWindowExt write FWindowExt;
    property WindowOrg: TPoint read FWindowOrg write FWindowOrg;
  end;

  { TdxPSPDFMetaFileTextInfo }

  TdxPSPDFMetaFileTextInfo = class(TObject)
  strict private
    FFont: TFont;
    FOptions: Integer;
    FText: string;
    FTextBounds: TRect;
    FTextOrigin: TPoint;
    FTextWidth: Integer;

    function CalculateTextWidth(const R: PEMRExtTextOut): Integer;
    function GetNoClip: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Concatenate(AInfo: TdxPSPDFMetaFileTextInfo): Boolean;
    procedure Initialize(const R: PEMRExtTextOut; AIsUnicode: Boolean; AFont: TFont);
    //
    property Font: TFont read FFont;
    property NoClip: Boolean read GetNoClip;
    property Options: Integer read FOptions;
    property Text: string read FText;
    property TextBounds: TRect read FTextBounds;
    property TextOrigin: TPoint read FTextOrigin;
    property TextWidth: Integer read FTextWidth;
  end;

  { TdxPSPDFMetaFileExportProvider }

  TdxPSPDFMetaFileExportProvider = class(TObject)
  strict private
    FBaseClipRegion: TdxPSPDFCanvasClipRegion;
    FBaseWindowOrg: TPoint;
    FBaseWorldTransform: TXForm;
    FGdiObjectList: TdxPSPDFMetaFileGdiObjectList;
    FMoveToPoint: TPoint;
    FPathCount: Integer;
    FPDFCanvas: TdxPSPDFReportRenderCanvas;
    FPrevTextOut: TdxPSPDFMetaFileTextInfo;
    FSavedStates: TcxObjectList;
    FScaleFactor: Single;
    FState: TdxPSPDFMetaFileCanvasState;

    function GetActualWorldTransform: TXForm;
    function GetBackgroundColor: TColor;
    function GetBrush: TBrush;
    function GetHasBackground: Boolean;
    function GetPageContent: TdxPSPDFPageContent;
    function GetPatternColor: TColor;
    function GetPen: TPen;
    function GetPenWidth: Integer;
    procedure OutputPrevTextInfo;
    procedure SelectStockObject(AIndex: Integer);
  protected
    function CheckROP(ARop: Cardinal): Boolean;
    function CreateRegion(ARgnData: PRgnData; ARgnDataSize: Integer): TcxRegion;
    function ExtractBitmap(ARecordAddress: Pointer; const ARect: TRect;
      ABitmapInfoOffset, ABitmapBitsOffset, ABitmapBitsSize, AColorUsage: Cardinal): TcxBitmap;
    procedure DrawBitmap(const ADest, ASource: TRect; ARecordAddress: Pointer;
      ABitmapInfoOffset, ABitmapBitsOffset, ABitmapBitsSize, AColorUsage: Cardinal);
    procedure DrawPicture(APicture: TGraphic; const ARect: TRect);
    procedure TextOut(AInfo: TdxPSPDFMetaFileTextInfo);
    //
    function ConvertPoint(const P: TPoint): TPoint;
    function ConvertPoints(P: PByte; ACount: Integer; AIsSmallPoints: Boolean): TPoints;
    function ConvertRect(const R: TRect): TRect;
    function ConvertRegion(ARegion: HRGN): TcxRegion;
    function ConvertValue(AValue: Integer; const AMapScale: Double): Integer;
    function ConvertValueX(AValue: Integer): Integer;
    function ConvertValueY(AValue: Integer): Integer;
    //
    procedure BeginOutput;
    procedure EndOutput;
    //
    procedure EmfAlphaBlend(const ARecord: PEMRAlphaBlend);
    procedure EmfBitBlt(const ARecord: PEMRBitBlt);
    procedure EmfCreateBrush(const ARecord: PEMRCreateBrushIndirect; ATable: PHandleTable);
    procedure EmfCreateFont(const ARecord: PEMRExtCreateFontIndirect; ATable: PHandleTable);
    procedure EmfCreatePen(const ARecord: PEMRCreatePen; ATable: PHandleTable);
    procedure EmfCreatePenEx(const ARecord: PEMRExtCreatePen; ATable: PHandleTable);
    procedure EmfDeleteObject(const ARecord: PEMRDeleteObject; ATable: PHandleTable);
    procedure EmfEllipse(R: TRect);
    procedure EmfExcludeClipRect(const ARecord: PEMRExcludeClipRect);
    procedure EmfExtTextOut(const ARecord: PEMRExtTextOut; AIsUnicode: Boolean);
    procedure EmfFillRgn(const ARecord: TEMRFillRgn; ATable: PHandleTable);
    procedure EmfIntersectClipRect(const ARecord: PEMRIntersectClipRect);
    procedure EmfLineTo(const ARecord: TEMRLineTo);
    procedure EmfMaskBlt(const ARecord: PEMRMaskBlt);
    procedure EmfModifyWorldTransform(const AInfo: PEMRModifyWorldTransform);
    procedure EmfPaintRgn(const ARecord: EMRPaintRgn);
    procedure EmfPie(const ARecord: EMRPie; ATable: PHandleTable);
    procedure EmfPolygon(const APolygon: PEMRPolyline; ASmallPoints: Boolean);
    procedure EmfPolyline(const P: PEMRPolyline; ASmallPoints: Boolean);
    procedure EmfPolyPolygon(const APolygon: PEMRPolyPolygon; ASmallPoints: Boolean);
    procedure EmfPolyPolyline(const APolyline: PEMRPolyPolyline; ASmallPoints: Boolean);
    procedure EmfRectangle(R: TRect);
    procedure EmfRestoreState;
    procedure EmfRoundRect(R: TRect; ACorners: TSize);
    procedure EmfSaveState;
    procedure EmfSelectClipRgn(const ARecord: PEMRExtSelectClipRgn);
    procedure EmfSelectObject(const ARecord: PEMRSelectObject; ATable: PHandleTable);
    procedure EmfSetPixel(const ARecord: TEMRSetPixelV);
    procedure EmfSetWorldTransform(const AInfo: PEMRSetWorldTransform);
    procedure EmfStretctBlt(const ARecord: PEMRStretchBlt);
    procedure EmfStretctDIBits(const ARecord: PEMRStretchDIBits);

    procedure ProcessMetaFileObject(ATable: PHandleTable; AObjectsInTable: Integer; const ARecord: PEnhMetaRecord);

    property BackgroundColor: TColor read GetBackgroundColor;
    property BaseWindowOrg: TPoint read FBaseWindowOrg;
    property Brush: TBrush read GetBrush;
    property GdiObjectList: TdxPSPDFMetaFileGdiObjectList read FGdiObjectList;
    property HasBackground: Boolean read GetHasBackground;
    property PageContent: TdxPSPDFPageContent read GetPageContent;
    property PatternColor: TColor read GetPatternColor;
    property Pen: TPen read GetPen;
    property PenWidth: Integer read GetPenWidth;
    property ScaleFactor: Single read FScaleFactor;
    property State: TdxPSPDFMetaFileCanvasState read FState;
  public
    constructor Create(APDFCanvas: TdxPSPDFReportRenderCanvas); virtual;
    destructor Destroy; override;
    procedure Render(AMetaFile: TMetafile; R: TRect; AScaleFactor: Single); virtual;
    //
    property PDFCanvas: TdxPSPDFReportRenderCanvas read FPDFCanvas;
  end;

procedure dxPDFExportMetaFile(APDFCanvas: TdxPSPDFReportRenderCanvas; AMetaFile: TMetafile; const R: TRect);
implementation

uses
  dxCoreGraphics, Math, dxPSTrueTypeFont, dxPSUtl;

const
  ROP_NOOP = $AA0029;
  PointSizeMap: array[Boolean] of Cardinal = (SizeOf(TPoint), SizeOf(TSmallPoint));

procedure dxPDFExportMetaFile(APDFCanvas: TdxPSPDFReportRenderCanvas; AMetaFile: TMetafile; const R: TRect);
begin
  if not IsRectEmpty(R) then
  begin
    with TdxPSPDFMetaFileExportProvider.Create(APDFCanvas) do
    try
      Render(AMetaFile, R, cxRectWidth(R) / AMetaFile.Width);
    finally
      Free;
    end;
  end;
end;

function EnhMetaFileProc(DC: HDC; ATable: PHandleTable;
  const ARecord: PEnhMetaRecord; AObjectsCount: Integer;
  AProvider: TdxPSPDFMetaFileExportProvider): Integer; stdcall;
begin
  AProvider.ProcessMetaFileObject(ATable, AObjectsCount, ARecord);
  Result := 1;
end;

{ TdxPSPDFMetaFileExportProvider }

constructor TdxPSPDFMetaFileExportProvider.Create(APDFCanvas: TdxPSPDFReportRenderCanvas);
begin
  inherited Create;
  FPDFCanvas := APDFCanvas;
  FSavedStates := TcxObjectList.Create;
  FGdiObjectList := TdxPSPDFMetaFileGdiObjectList.Create;
  FState := TdxPSPDFMetaFileCanvasState.Create;
end;

destructor TdxPSPDFMetaFileExportProvider.Destroy;
begin
  FreeAndNil(FState);
  FreeAndNil(FSavedStates);
  FreeAndNil(FGdiObjectList);
  FreeAndNil(FBaseClipRegion);
  inherited Destroy;
end;

procedure TdxPSPDFMetaFileExportProvider.Render(AMetaFile: TMetafile; R: TRect; AScaleFactor: Single);
begin
  FreeAndNil(FBaseClipRegion);

  PDFCanvas.SaveState;
  try
    FScaleFactor := AScaleFactor;
    R := PDFCanvas.AbsoluteRect(R);
    FBaseWindowOrg := cxPointInvert(R.TopLeft);
    FBaseWorldTransform := PDFCanvas.WorldTransform;
    FBaseClipRegion := PDFCanvas.ClipRegion.Clone;
    FBaseClipRegion.Intersect(R, FBaseWorldTransform);

    State.PolyFillMode := ALTERNATE;
    State.WindowExt := cxSize(AMetaFile.Width, AMetaFile.Height);
    State.WindowOrg := cxNullPoint;
    State.ViewPortExt := State.WindowExt;
    EnumEnhMetaFile(0, AMetaFile.Handle, @EnhMetaFileProc, Self, cxNullRect);
  finally
    PDFCanvas.RestoreState;
  end;
end;

function TdxPSPDFMetaFileExportProvider.ConvertPoint(const P: TPoint): TPoint;
begin
  Result.X := ConvertValueX(P.X);
  Result.Y := ConvertValueY(P.Y);
end;

function TdxPSPDFMetaFileExportProvider.ConvertPoints(P: PByte; ACount: Integer; AIsSmallPoints: Boolean): TPoints;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
  begin
    if AIsSmallPoints then
      Result[I] := ConvertPoint(SmallPointToPoint(PSmallPoint(P)^))
    else
      Result[I] := ConvertPoint(PPoint(P)^);
    Inc(P, PointSizeMap[AIsSmallPoints]);
  end;
end;

function TdxPSPDFMetaFileExportProvider.ConvertRect(const R: TRect): TRect;
begin
  Result.TopLeft := ConvertPoint(R.TopLeft);
  Result.BottomRight := ConvertPoint(R.BottomRight);
end;

function TdxPSPDFMetaFileExportProvider.ConvertRegion(ARegion: HRGN): TcxRegion;
var
  ARgnData: PRgnData;
  ARgnDataSize: Integer;
begin
  ARgnDataSize := GetRegionData(ARegion, 0, nil);
  ARgnData := AllocMem(ARgnDataSize);
  try
    if GetRegionData(ARegion, ARgnDataSize, ARgnData) <> 0 then
      Result := CreateRegion(ARgnData, ARgnDataSize)
    else
      Result := nil;
  finally
    FreeMem(ARgnData, ARgnDataSize);
  end;
  DeleteObject(ARegion);
end;

function TdxPSPDFMetaFileExportProvider.ConvertValue(AValue: Integer; const AMapScale: Double): Integer;
var
  ATempValue: Double;
begin
  ATempValue := AValue;
  case State.MapMode of
    MM_ANISOTROPIC, MM_ISOTROPIC:
      ATempValue := ATempValue * AMapScale;
  end;
  Result := Round(ScaleFactor * ATempValue);
end;

function TdxPSPDFMetaFileExportProvider.ConvertValueX(AValue: Integer): Integer;
begin
  Result := ConvertValue(AValue, State.ViewPortExt.cx / State.WindowExt.cx);
end;

function TdxPSPDFMetaFileExportProvider.ConvertValueY(AValue: Integer): Integer;
begin
  Result := ConvertValue(AValue, State.ViewPortExt.cy / State.WindowExt.cy);
end;

function TdxPSPDFMetaFileExportProvider.CheckROP(ARop: Cardinal): Boolean;
begin
  Result := ARop and ROP_NOOP <> ROP_NOOP;
end;

function TdxPSPDFMetaFileExportProvider.CreateRegion(ARgnData: PRgnData; ARgnDataSize: Integer): TcxRegion;
var
  ARgnRect: PRect;
  I: Integer;
begin
  Result := TcxRegion.Create(cxNullRect);
  ARgnRect := PRect(@ARgnData^.Buffer[0]);
  for I := 0 to ARgnData^.rdh.nCount - 1 do
  begin
    Result.Combine(ConvertRect(ARgnRect^), roAdd);
    Inc(ARgnRect);
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.BeginOutput;
begin
  PDFCanvas.SaveState;
  PDFCanvas.WorldTransform := GetActualWorldTransform;
  PDFCanvas.WindowOrg := cxPointOffset(cxPointOffset(State.WindowOrg, State.ViewPortOrg, False), BaseWindowOrg);
end;

procedure TdxPSPDFMetaFileExportProvider.EndOutput;
begin
  PDFCanvas.RestoreState;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfAlphaBlend(const ARecord: PEMRAlphaBlend);
var
  ASourceBitmap, ABitmap: TcxBitmap;
begin
  if CheckROP(ARecord^.dwRop) then
  begin
    ASourceBitmap := ExtractBitmap(ARecord,
      Bounds(ARecord^.xSrc, ARecord^.ySrc, ARecord^.cxSrc, ARecord^.cySrc),
      ARecord^.offBmiSrc, ARecord^.offBitsSrc, ARecord^.cbBitsSrc, ARecord^.iUsageSrc);
    try
      ABitmap := TcxBitmap.CreateSize(ASourceBitmap.ClientRect);
      try
        ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ARecord^.crBkColorSrc);
        if cxGetBitmapPixelFormat(ASourceBitmap) = 32 then
          cxAlphaBlend(ABitmap, ASourceBitmap, ABitmap.ClientRect, ASourceBitmap.ClientRect)
        else
        begin
          ASourceBitmap.Transparent := True;
          ABitmap.Canvas.Draw(0, 0, ASourceBitmap);
        end;
        DrawPicture(ABitmap, Bounds(ARecord^.xDest, ARecord^.yDest, ARecord^.cxDest, ARecord^.cyDest));
      finally
        ABitmap.Free;
      end;
    finally
      ASourceBitmap.Free;
    end;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfBitBlt(const ARecord: PEMRBitBlt);
var
  R: TRect;
begin
  if CheckROP(ARecord^.dwRop) then
  begin
    R := cxRectAdjust(Bounds(ARecord^.xDest, ARecord^.yDest, ARecord^.cxDest, ARecord^.cyDest));
    if ARecord.cbBitsSrc = 0 then
    begin
      BeginOutput;
      try
        PDFCanvas.FillRect(ConvertRect(R), PatternColor);
      finally
        EndOutput;
      end;
    end
    else
      DrawBitmap(R, Bounds(ARecord^.xSrc, ARecord^.ySrc, cxRectWidth(R), cxRectHeight(R)),
        ARecord, ARecord^.offBmiSrc, ARecord^.offBitsSrc, ARecord^.cbBitsSrc, ARecord^.iUsageSrc);
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfCreateBrush(const ARecord: PEMRCreateBrushIndirect; ATable: PHandleTable);
begin
  ATable^.objectHandle[ARecord^.ihBrush] := GdiObjectList.CreateBrush(ARecord^.lb);
end;

procedure TdxPSPDFMetaFileExportProvider.EmfCreateFont(const ARecord: PEMRExtCreateFontIndirect; ATable: PHandleTable);
begin
  ATable^.objectHandle[ARecord^.ihFont] := GdiObjectList.CreateFont(ARecord^.elfw);
end;

procedure TdxPSPDFMetaFileExportProvider.EmfCreatePen(const ARecord: PEMRCreatePen; ATable: PHandleTable);
begin
  ATable^.objectHandle[ARecord^.ihPen] := GdiObjectList.CreatePen(ARecord^.lopn);
end;

procedure TdxPSPDFMetaFileExportProvider.EmfCreatePenEx(const ARecord: PEMRExtCreatePen; ATable: PHandleTable);
begin
  ATable^.objectHandle[ARecord^.ihPen] := GdiObjectList.CreatePenEx(ARecord^.elp);
end;

procedure TdxPSPDFMetaFileExportProvider.EmfDeleteObject(const ARecord: PEMRDeleteObject; ATable: PHandleTable);
var
  AObject: TdxPSPDFMetaFileGdiObjectItem;
begin
  if GdiObjectList.FindObject(ATable^.ObjectHandle[ARecord^.ihObject], AObject) then
    GdiObjectList.Remove(AObject);
end;

procedure TdxPSPDFMetaFileExportProvider.EmfExtTextOut(const ARecord: PEMRExtTextOut; AIsUnicode: Boolean);
var
  AInfo: TdxPSPDFMetaFileTextInfo;
begin
  if ARecord^.emrtext.nChars > 0 then
  begin
    AInfo := TdxPSPDFMetaFileTextInfo.Create;
    AInfo.Initialize(ARecord, AIsUnicode, PDFCanvas.Font);

    if (FPrevTextOut <> nil) and FPrevTextOut.Concatenate(AInfo) then
      FreeAndNil(AInfo)
    else
    begin
      OutputPrevTextInfo;
      FPrevTextOut := AInfo;
    end;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfEllipse(R: TRect);
begin
  BeginOutput;
  try
    R := ConvertRect(R);
    if HasBackground then
      PDFCanvas.FillEllipse(R, BackgroundColor, State.TextColor, nil, Brush);
    PDFCanvas.DrawEllipseFrame(R, Pen.Color, PenWidth);
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfRestoreState;
var
  ATempState: TdxPSPDFMetaFileCanvasState;
begin
  PDFCanvas.RestoreState;
  ATempState := TdxPSPDFMetaFileCanvasState(FSavedStates.Last);
  FSavedStates.Delete(FSavedStates.Count - 1);
  State.Assign(ATempState);
  ATempState.Free;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfSaveState;
begin
  PDFCanvas.SaveState;
  FSavedStates.Add(State.Clone);
end;

procedure TdxPSPDFMetaFileExportProvider.EmfFillRgn(const ARecord: TEMRFillRgn; ATable: PHandleTable);
var
  AObject: TdxPSPDFMetaFileGdiObjectItem;
  ARegion: TcxRegion;
begin
  if GdiObjectList.FindObject(ATable^.ObjectHandle[ARecord.ihBrush], AObject) then
  begin
    ARegion := CreateRegion(@ARecord.RgnData[0], ARecord.cbRgnData);
    try
      BeginOutput;
      try
        PDFCanvas.FillRegion(ARegion.Handle, AObject.Color, clNone, TdxPSSolidFillPattern, nil);
      finally
        EndOutput;
      end;
    finally
      ARegion.Free;
    end;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfExcludeClipRect(const ARecord: PEMRExcludeClipRect);
begin
  BeginOutput;
  try
    PDFCanvas.ExcludeClipRect(ConvertRect(ARecord^.rclClip));
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfIntersectClipRect(const ARecord: PEMRIntersectClipRect);
begin
  BeginOutput;
  try
    PDFCanvas.IntersectClipRgn(ConvertRect(ARecord^.rclClip));
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfLineTo(const ARecord: TEMRLineTo);
begin
  BeginOutput;
  try
    PDFCanvas.Polyline([ConvertPoint(FMoveToPoint), ConvertPoint(ARecord.ptl)], Pen.Color, PenWidth);
    FMoveToPoint := ARecord.ptl;
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfMaskBlt(const ARecord: PEMRMaskBlt);

  procedure SetMaskRegion(AMask: TcxBitmap; ATransparentColor: TColor);
  var
    AMaskRegion: TcxRegion;
    AOffset: TPoint;
  begin
    AMaskRegion := ConvertRegion(cxCreateRegionFromBitmap(AMask, ATransparentColor));
    if AMaskRegion <> nil then
    try
      AOffset := ConvertPoint(Point(ARecord^.xDest, ARecord^.yDest));
      AOffset := cxPointOffset(AOffset, cxPointOffset(State.WindowOrg, BaseWindowOrg), False);
      AMaskRegion.Offset(AOffset);
      PDFCanvas.ClipRegion.IntersectEx(AMaskRegion, GetActualWorldTransform);
    finally
      AMaskRegion.Free;
    end;
  end;

  function PrepareBitmap(const ARect: TRect): TcxBitmap;
  var
    ABitmap: TcxBitmap;
  begin
    Result := TcxBitmap.CreateSize(ARect, pf24bit);
    Result.cxCanvas.FillRect(Result.ClientRect, ARecord^.crBkColorSrc);

    ABitmap := ExtractBitmap(ARecord, Bounds(0, 0, ARecord^.cxDest, ARecord^.cyDest),
      ARecord^.offBmiSrc, ARecord^.offBitsSrc, ARecord^.cbBitsSrc, ARecord^.iUsageSrc);
    try
      ABitmap.Transparent := True;
      Result.Canvas.Draw(0, 0, ABitmap);
      SetMaskRegion(Result, ARecord^.crBkColorSrc);
    finally
      ABitmap.Free;
    end;
  end;

var
  ABitmap: TcxBitmap;
  ARect: TRect;
begin
  if CheckROP(ARecord^.dwRop) then
  begin
    PDFCanvas.SaveClipRgn;
    try
      ARect := Bounds(ARecord^.xDest, ARecord^.yDest, ARecord^.cxDest, ARecord^.cyDest);
      ABitmap := PrepareBitmap(ARect);
      try
        DrawPicture(ABitmap, ARect);
      finally
        ABitmap.Free;
      end;
    finally
      PDFCanvas.RestoreClipRgn;
    end;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfModifyWorldTransform(const AInfo: PEMRModifyWorldTransform);
begin
  case AInfo^.iMode of
    MWT_LEFTMULTIPLY:
      State.Transform := TXForm.Combine(AInfo^.xform, State.Transform);
    MWT_RIGHTMULTIPLY:
      State.Transform := TXForm.Combine(State.Transform, AInfo^.xform);
  else
    State.Transform := AInfo^.xform;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfPaintRgn(const ARecord: EMRPaintRgn);
var
  ARegion: TcxRegion;
begin
  if HasBackground then
  begin
    ARegion := CreateRegion(@ARecord.RgnData[0], ARecord.cbRgnData);
    try
      BeginOutput;
      try
        PDFCanvas.FillRegion(ARegion.Handle, BackgroundColor, clNone, nil, Brush);
      finally
        EndOutput;
      end;
    finally
      ARegion.Free;
    end;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfPolyline(const P: PEMRPolyline; ASmallPoints: Boolean);
begin
  BeginOutput;
  try
    PDFCanvas.Polyline(ConvertPoints(@P^.aptl[0], P^.cptl, ASmallPoints), Pen.Color, PenWidth);
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfPolyPolygon(const APolygon: PEMRPolyPolygon; ASmallPoints: Boolean);
var
  ACount: Integer;
  ACursor: PByte;
  I: Integer;
begin
  BeginOutput;
  try
    ACount := Integer(APolygon^.nPolys);
    ACursor := @APolygon.aPolyCounts[0];
    Inc(ACursor, SizeOf(Cardinal) * ACount);

    PDFCanvas.PreparePageContent;
    try
      PageContent.SelectLineWidth(PenWidth);
      for I := 0 to ACount - 1 do
      begin
        if APolygon^.aPolyCounts[I] > 1 then
          PageContent.WritePoints(ConvertPoints(ACursor, APolygon^.aPolyCounts[I], ASmallPoints));
        Inc(ACursor, APolygon^.aPolyCounts[I] * PointSizeMap[ASmallPoints]);
      end;
      PageContent.FillPolygon(Pen.Color, BackgroundColor, State.PolyFillMode);
    finally
      PDFCanvas.UnpreparePageContent;
    end;
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfPolyPolyline(const APolyline: PEMRPolyPolyline; ASmallPoints: Boolean);
var
  ACount: Integer;
  ACursor: PByte;
  I: Integer;
begin
  BeginOutput;
  try
    ACount := Integer(APolyline^.nPolys);
    ACursor := @APolyline.aPolyCounts[0];
    Inc(ACursor, SizeOf(Cardinal) * ACount);

    PDFCanvas.PreparePageContent;
    try
      for I := 0 to ACount - 1 do
      begin
        PageContent.Polyline(ConvertPoints(ACursor, APolyline^.aPolyCounts[I], ASmallPoints), PenWidth, Pen.Color);
        Inc(ACursor, APolyline^.aPolyCounts[I] * PointSizeMap[ASmallPoints]);
      end;
    finally
      PDFCanvas.UnpreparePageContent;
    end;
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfPie(const ARecord: EMRPie; ATable: PHandleTable);
begin
  BeginOutput;
  try
    PDFCanvas.Pie(ConvertRect(ARecord.rclBox), ConvertPoint(ARecord.ptlStart), ConvertPoint(ARecord.ptlEnd), BackgroundColor);
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfPolygon(const APolygon: PEMRPolyline; ASmallPoints: Boolean);
begin
  BeginOutput;
  try
    PDFCanvas.Polygon(
      ConvertPoints(@APolygon^.aptl[0], APolygon^.cptl, ASmallPoints),
      Pen.Color, BackgroundColor, PenWidth, State.PolyFillMode);
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfRectangle(R: TRect);
begin
  BeginOutput;
  try
    R := ConvertRect(R);
    if HasBackground then
      PDFCanvas.FillRectEx(R, BackgroundColor, nil, Brush);
    if (BackgroundColor <> Pen.Color) and (Pen.Width <> 0) then
      PDFCanvas.DrawFrame(R, Pen.Color, Pen.Color, PenWidth);
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfRoundRect(R: TRect; ACorners: TSize);
begin
  BeginOutput;
  try
    R := ConvertRect(R);
    ACorners.cx := ConvertValueX(ACorners.cx div 2);
    ACorners.cy := ConvertValueY(ACorners.cy div 2);
    if HasBackground then
      PDFCanvas.FillRoundRect(R, ACorners.cx, ACorners.cy, BackgroundColor, State.TextColor, nil, Brush);
    PDFCanvas.DrawRoundFrame(R, ACorners.cx, ACorners.cy, Pen.Color, PenWidth);
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfSelectClipRgn(const ARecord: PEMRExtSelectClipRgn);
var
  ARegion: TcxRegion;
begin
  PDFCanvas.ClipRegion := FBaseClipRegion.Clone;
  if ARecord.cbRgnData <> 0 then
  begin
    ARegion := CreateRegion(@ARecord.RgnData[0], ARecord.cbRgnData);
    try
      ARegion.Offset(cxPointInvert(BaseWindowOrg));
      PDFCanvas.ClipRegion.IntersectEx(ARegion, FBaseWorldTransform{GetActualWorldTransform});
    finally
      ARegion.Free;
    end;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfSelectObject(const ARecord: PEMRSelectObject; ATable: PHandleTable);
var
  AObject: TdxPSPDFMetaFileGdiObjectItem;
begin
  if ARecord^.ihObject and $80000000 <> 0 then
    SelectStockObject(ARecord^.ihObject and $7FFFFFFF)
  else
    if GdiObjectList.FindObject(ATable^.ObjectHandle[ARecord^.ihObject], AObject) then
    begin
      if AObject.SourceObject is TBrush then
      begin
        Brush.Assign(TBrush(AObject.SourceObject));
        Brush.Color := AObject.Color;
      end
      else

      if AObject.SourceObject is TFont then
      begin
        PDFCanvas.Font := TFont(AObject.SourceObject);
        PDFCanvas.Font.Size := ConvertValueX(PDFCanvas.Font.Size);
      end
      else

      if AObject.SourceObject is TPen then
        Pen.Assign(TPen(AObject.SourceObject));
    end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfSetPixel(const ARecord: TEMRSetPixelV);
begin
  BeginOutput;
  try
    PDFCanvas.FillRect(ConvertRect(Bounds(ARecord.ptlPixel.X, ARecord.ptlPixel.Y, 1, 1)), ARecord.crColor);
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfSetWorldTransform(const AInfo: PEMRSetWorldTransform);
begin
  State.Transform := AInfo^.xform;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfStretctBlt(const ARecord: PEMRStretchBlt);
begin
  if (ARecord^.cbBitsSrc <> 0) and CheckROP(ARecord^.dwRop) then
  begin
    DrawBitmap(
      Bounds(ARecord^.xDest, ARecord^.yDest, ARecord^.cxDest, ARecord^.cyDest),
      Bounds(ARecord^.xSrc, ARecord^.ySrc, ARecord^.cxSrc, ARecord^.cySrc),
      ARecord, ARecord^.offBmiSrc, ARecord^.offBitsSrc, ARecord^.cbBitsSrc,
      ARecord^.iUsageSrc);
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.EmfStretctDIBits(const ARecord: PEMRStretchDIBits);
begin
  if (ARecord^.cbBitsSrc <> 0) and CheckROP(ARecord^.dwRop) then
  begin
    DrawBitmap(
      Bounds(ARecord^.xDest, ARecord^.yDest, ARecord^.cxDest, ARecord^.cyDest),
      Bounds(ARecord^.xSrc, ARecord^.ySrc, ARecord^.cxSrc, ARecord^.cySrc),
      ARecord, ARecord^.offBmiSrc, ARecord^.offBitsSrc, ARecord^.cbBitsSrc,
      ARecord^.iUsageSrc);
  end;
end;

function TdxPSPDFMetaFileExportProvider.ExtractBitmap(ARecordAddress: Pointer; const ARect: TRect;
  ABitmapInfoOffset, ABitmapBitsOffset, ABitmapBitsSize, AColorUsage: Cardinal): TcxBitmap;

  function GetPixelFormat(const AInfo: PBitmapInfo): TPixelFormat;
  begin
    case AInfo^.bmiHeader.biBitCount of
      1: Result := pf1bit;
      8: Result := pf8bit;
      16: Result := pf16bit;
      32: Result := pf32bit;
      else
        Result := pf24bit;
    end;
  end;

var
  ABits: Pointer;
  AHandle: HBITMAP;
  AInfo: PBitmapInfo;
begin
  AInfo := PBitmapInfo(TdxNativeUInt(ARecordAddress) + ABitmapInfoOffset);
  AHandle := CreateDIBSection(cxScreenCanvas.Handle, AInfo^, DIB_RGB_COLORS, ABits, 0, 0);
  try
    cxCopyData(Pointer(TdxNativeUInt(ARecordAddress) + ABitmapBitsOffset), ABits, ABitmapBitsSize);
    Result := TcxBitmap.CreateSize(ARect, GetPixelFormat(AInfo));
    StretchDIBits(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height,
      ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect),
      ABits, AInfo^, AColorUsage, SRCCOPY);
  finally
    DeleteObject(AHandle);
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.TextOut(AInfo: TdxPSPDFMetaFileTextInfo);
var
  ATempFont: TFont;
  AText: string;
  ATextOrigin: TdxPointF;
  ATextRect: TRect;
  ATextWidth: Integer;
begin
  BeginOutput;
  try
    ATextOrigin := dxPointF(ConvertPoint(AInfo.TextOrigin));
    ATextRect := ConvertRect(AInfo.TextBounds);
    if AInfo.NoClip and (AInfo.Font.Orientation = 0) then
      ATextWidth := cxRectWidth(ATextRect) + 1
    else
      ATextWidth := ConvertValueX(AInfo.TextWidth);

    AText := AInfo.Text;
    TdxPDFBiDiHelper.CheckString(AText);
    if dxAreFontsEqual(PDFCanvas.Font, AInfo.Font) then
      PDFCanvas.DrawTextLine(AText, ATextOrigin, ATextRect, ATextWidth, State.TextColor, 0, 0, AInfo.NoClip)
    else
    begin
      ATempFont := TFont.Create;
      try
        ATempFont.Assign(PDFCanvas.Font);
        PDFCanvas.Font := AInfo.Font;
        PDFCanvas.DrawTextLine(AText, ATextOrigin, ATextRect, ATextWidth, State.TextColor, 0, 0, AInfo.NoClip);
        PDFCanvas.Font := ATempFont;
      finally
        ATempFont.Free;
      end;
    end;
  finally
    EndOutput;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.DrawBitmap(const ADest, ASource: TRect;
  ARecordAddress: Pointer; ABitmapInfoOffset, ABitmapBitsOffset, ABitmapBitsSize, AColorUsage: Cardinal);
var
  ABitmap: TcxBitmap;
begin
  ABitmap := ExtractBitmap(ARecordAddress, ASource, ABitmapInfoOffset, ABitmapBitsOffset, ABitmapBitsSize, AColorUsage);
  try
    DrawPicture(ABitmap, ADest);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.DrawPicture(APicture: TGraphic; const ARect: TRect);
begin
  BeginOutput;
  try
    PDFCanvas.DrawPicture(APicture, ConvertRect(ARect), ppmStretch, PixelsNumerator, PixelsDenominator);
  finally
    EndOutput;
  end;
end;

function TdxPSPDFMetaFileExportProvider.GetActualWorldTransform: TXForm;
begin
  Result := State.Transform;
  Result.eDx := Result.eDx * ScaleFactor;
  Result.eDy := Result.eDy * ScaleFactor;
  Result := TXForm.Combine(FBaseWorldTransform, Result);
end;

function TdxPSPDFMetaFileExportProvider.GetBackgroundColor: TColor;
begin
  if Brush.Style <> bsClear then
    Result := Brush.Color
  else
    Result := clNone;
end;

function TdxPSPDFMetaFileExportProvider.GetBrush: TBrush;
begin
  Result := State.Brush;
end;

function TdxPSPDFMetaFileExportProvider.GetHasBackground: Boolean;
begin
  Result := (BackgroundColor <> clNone) or (Brush.Style > bsClear);
end;

function TdxPSPDFMetaFileExportProvider.GetPageContent: TdxPSPDFPageContent;
begin
  Result := PDFCanvas.PageContent;
end;

function TdxPSPDFMetaFileExportProvider.GetPatternColor: TColor;
begin
  Result := BackgroundColor;
  if Result = clNone then
    Result := State.TextColor;
end;

function TdxPSPDFMetaFileExportProvider.GetPen: TPen;
begin
  Result := State.Pen;
end;

function TdxPSPDFMetaFileExportProvider.GetPenWidth: Integer;
var
  AWidth: Single;
begin
  AWidth := 1;// State.Transform.eM11;
  if State.MapMode <> MM_TEXT then
    AWidth := AWidth * State.ViewPortExt.cx / State.WindowExt.cx;
  if Pen.Width <> 0 then
    AWidth := AWidth * Pen.Width;
  Result := Round(AWidth * ScaleFactor);
end;

procedure TdxPSPDFMetaFileExportProvider.OutputPrevTextInfo;
begin
  if FPrevTextOut <> nil then
  try
    TextOut(FPrevTextOut);
  finally
    FreeAndNil(FPrevTextOut);
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.SelectStockObject(AIndex: Integer);
begin
  case AIndex of
    BLACK_BRUSH:
      Brush.Color := clBlack;
    BLACK_PEN:
      Pen.Color := clBlack;
    DKGRAY_BRUSH:
      Brush.Color := $666666;
    GRAY_BRUSH:
      Brush.Color := clGray;
    LTGRAY_BRUSH:
      Brush.Color := $AAAAAA;
    NULL_BRUSH:
      Brush.Style := bsClear;
    NULL_PEN:
      Pen.Color := clNone;
    WHITE_BRUSH:
      Brush.Color := clWhite;
    WHITE_PEN:
      Pen.Color := clWhite;
    OEM_FIXED_FONT, ANSI_FIXED_FONT, ANSI_VAR_FONT, SYSTEM_FONT:
      PDFCanvas.Font.Name := 'Arial';
  end;
end;

procedure TdxPSPDFMetaFileExportProvider.ProcessMetaFileObject(
  ATable: PHandleTable; AObjectsInTable: Integer; const ARecord: PEnhMetaRecord);
begin
  case ARecord^.iType of
    EMR_EXTCREATEFONTINDIRECTW:
      EmfCreateFont(PEMRExtCreateFontIndirect(ARecord), ATable);
    EMR_SELECTOBJECT:
      EmfSelectObject(PEMRSelectObject(ARecord), ATable);
    EMR_DELETEOBJECT:
      EmfDeleteObject(PEMRDeleteObject(ARecord), ATable);
    EMR_EXTTEXTOUTW, EMR_EXTTEXTOUTA:
      EmfExtTextOut(PEMRExtTextOut(ARecord), ARecord^.iType = EMR_EXTTEXTOUTW);
  else
    OutputPrevTextInfo;
  end;

  case ARecord^.iType of
    EMR_BEGINPATH:
      Inc(FPathCount);
    EMR_ENDPATH:
      Dec(FPathCount);
  end;
  if FPathCount <> 0 then
    Exit;

  case ARecord^.iType of
    EMR_MASKBLT:
      EmfMaskBlt(PEMRMaskBlt(ARecord));
    EMR_BITBLT:
      EmfBitBlt(PEMRBitBlt(ARecord));
    EMR_STRETCHBLT:
      EmfStretctBlt(PEMRStretchBlt(ARecord));
    EMR_STRETCHDIBITS:
      EmfStretctDIBits(PEMRStretchDiBits(ARecord));
    EMR_ELLIPSE:
      EmfEllipse(PEMREllipse(ARecord)^.rclBox);
    EMR_RECTANGLE:
      EmfRectangle(PEMRRectangle(ARecord)^.rclBox);
    EMR_CREATEPEN:
      EmfCreatePen(PEMRCreatePen(ARecord), ATable);
    EMR_CREATEBRUSHINDIRECT:
      EmfCreateBrush(PEMRCreateBrushIndirect(ARecord), ATable);
    EMR_EXTCREATEPEN:
      EmfCreatePenEx(PEMRExtCreatePen(ARecord), ATable);
    EMR_ROUNDRECT:
      EmfRoundRect(PEMRRoundRect(ARecord)^.rclBox, PEMRRoundRect(ARecord)^.szlCorner);
    EMR_POLYGON, EMR_POLYGON16:
      EmfPolygon(PEMRPolyline(ARecord), ARecord^.iType = EMR_POLYGON16);
    EMR_POLYPOLYGON, EMR_POLYPOLYGON16:
      EmfPolyPolygon(PEMRPolyPolygon(ARecord), ARecord^.iType = EMR_POLYPOLYGON16);
    EMR_POLYPOLYLINE16, EMR_POLYPOLYLINE:
      EmfPolyPolyline(PEMRPolyPolyline(ARecord), ARecord^.iType = EMR_POLYPOLYLINE16);
    EMR_POLYLINE, EMR_POLYLINE16:
      EmfPolyLine(PEMRPolyline(ARecord), ARecord^.iType = EMR_POLYLINE16);
    EMR_SETPIXELV:
      EmfSetPixel(PEMRSetPixelV(ARecord)^);
    EMR_MOVETOEX:
      FMoveToPoint := PEMRMoveToEx(ARecord)^.ptl;
    EMR_LINETO:
      EmfLineTo(PEMRLineTo(ARecord)^);
    EMR_PAINTRGN:
      EmfPaintRgn(PEMRPaintRgn(ARecord)^);
    EMR_FILLRGN:
      EmfFillRgn(PEMRFillRgn(ARecord)^, ATable);
    EMR_PIE:
      EmfPie(PEMRPie(ARecord)^, ATable);
    EMR_ALPHABLEND:
      EmfAlphaBlend(PEMRAlphaBlend(ARecord));

    EMR_SETMAPMODE:
      State.MapMode := PEMRSetMapMode(ARecord)^.iMode;
    EMR_SETBKCOLOR:
      State.BackgroundColor := PEMRSETBKCOLOR(ARecord)^.crColor;
    EMR_SETTEXTCOLOR:
      State.TextColor := PEMRSETTEXTCOLOR(ARecord)^.crColor;
    EMR_SETBKMODE:
      State.BackgroundMode := PEMRSETBKMODE(ARecord)^.iMode;
    EMR_SETPOLYFILLMODE:
      State.PolyFillMode := PEMRSetPolyFillMode(ARecord)^.iMode;

    EMR_SAVEDC:
      EmfSaveState;
    EMR_RESTOREDC:
      EmfRestoreState;

    EMR_EXTSELECTCLIPRGN:
      EmfSelectClipRgn(PEMRExtSelectClipRgn(ARecord));
    EMR_EXCLUDECLIPRECT:
      EmfExcludeClipRect(PEMREXCLUDECLIPRECT(ARecord));
    EMR_INTERSECTCLIPRECT:
      EmfIntersectClipRect(PEMRINTERSECTCLIPRECT(ARecord));

    EMR_MODIFYWORLDTRANSFORM:
      EmfModifyWorldTransform(PEMRModifyWorldTransform(ARecord));
    EMR_SETWORLDTRANSFORM:
      EmfSetWorldTransform(PEMRSetWorldTransform(ARecord));
    EMR_SETVIEWPORTEXTEX:
      State.ViewPortExt := PEMRSetViewportExtEx(ARecord)^.szlExtent;
    EMR_SETVIEWPORTORGEX:
      State.ViewPortOrg := ConvertPoint(PEMRSetViewportOrgEx(ARecord)^.ptlOrigin);
    EMR_SETWINDOWEXTEX:
      State.WindowExt := PEMRSetWindowExtEx(ARecord)^.szlExtent;
    EMR_SETWINDOWORGEX:
      State.WindowOrg := ConvertPoint(PEMRSETWINDOWORGEX(ARecord).ptlOrigin);
    EMR_EOF:
      { do nothing };
  end;
end;

{ TdxPSPDFMetaFileGdiObjectList }

function TdxPSPDFMetaFileGdiObjectList.CreateBrush(
  const ABrushInfo: {$IFDEF DELPHI16}TLogBrush32{$ELSE}TLogBrush{$ENDIF}): HBRUSH;
var
  ABrush: TBrush;
  ABrushInfo1: TLogBrush;
begin
  ABrushInfo1.lbStyle := ABrushInfo.lbStyle;
  ABrushInfo1.lbColor := ABrushInfo.lbColor;
  ABrushInfo1.lbHatch := ABrushInfo.lbHatch;
  ABrush := TBrush.Create;
  ABrush.Handle := CreateBrushIndirect(ABrushInfo1);
  Add(TdxPSPDFMetaFileGdiObjectItem.Create(ABrush.Handle, ABrush, ABrushInfo.lbColor));
  Result := ABrush.Handle;
end;

function TdxPSPDFMetaFileGdiObjectList.CreateFont(const AFontInfo: EXTLOGFONTW): HFONT;
var
  AFont: TFont;
  ALogFont: TLogFontW;
begin
  ALogFont := AFontInfo.elfLogFont;
  if ALogFont.lfOrientation = 0 then
    ALogFont.lfOrientation := ALogFont.lfEscapement;

  AFont := TFont.Create;
  AFont.Handle := CreateFontIndirectW(ALogFont);
  Add(TdxPSPDFMetaFileGdiObjectItem.Create(AFont.Handle, AFont, clDefault));
  Result := AFont.Handle;
end;

function TdxPSPDFMetaFileGdiObjectList.CreatePen(const APenInfo: TLogPen): HPEN;
var
  APen: TPen;
begin
  APen := TPen.Create;
  APen.Color := APenInfo.lopnColor;
  APen.Width := APenInfo.lopnWidth.X;
  Add(TdxPSPDFMetaFileGdiObjectItem.Create(APen.Handle, APen, APenInfo.lopnColor));
  Result := APen.Handle;
end;

function TdxPSPDFMetaFileGdiObjectList.CreatePenEx(
  const APenInfo: {$IFDEF DELPHI16}TExtLogPen32{$ELSE}TExtLogPen{$ENDIF}): HPEN;
var
  APen: TPen;
begin
  APen := TPen.Create;
  APen.Color := APenInfo.elpColor;
  APen.Width := APenInfo.elpWidth;
  Add(TdxPSPDFMetaFileGdiObjectItem.Create(APen.Handle, APen, APenInfo.elpColor));
  Result := APen.Handle;
end;

function TdxPSPDFMetaFileGdiObjectList.FindObject(
  AHandle: THandle; var AObject: TdxPSPDFMetaFileGdiObjectItem): Boolean;
var
  I: Integer;
begin
  AObject := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].Handle = AHandle;
    if Result then
    begin
      AObject := Items[I];
      Break;
    end;
  end;
  Result := Assigned(AObject);
end;

function TdxPSPDFMetaFileGdiObjectList.GetItem(Index: Integer): TdxPSPDFMetaFileGdiObjectItem;
begin
  Result := TdxPSPDFMetaFileGdiObjectItem(inherited Items[Index]);
end;

procedure TdxPSPDFMetaFileGdiObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Assigned(Ptr) and (Action = lnDeleted) then
    TObject(Ptr).Free;
end;

{ TdxPSPDFMetaFileGdiObjectItem }

constructor TdxPSPDFMetaFileGdiObjectItem.Create(AHandle: THandle; ASourceObject: TObject; AColor: TColor);
begin
  inherited Create;
  FHandle := AHandle;
  FSourceObject := ASourceObject;
  FColor := AColor;
end;

destructor TdxPSPDFMetaFileGdiObjectItem.Destroy;
begin
  FreeAndNil(FSourceObject);
  inherited Destroy;
end;

{ TdxPSPDFMetaFileCanvasState }

constructor TdxPSPDFMetaFileCanvasState.Create;
begin
  inherited Create;
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  FWindowExt := cxSize(1, 1);
  FViewPortExt := cxSize(1, 1);
  FTransform := TXForm.CreateIdentityMatrix;
end;

destructor TdxPSPDFMetaFileCanvasState.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure TdxPSPDFMetaFileCanvasState.Assign(ASource: TdxPSPDFMetaFileCanvasState);
begin
  Pen.Assign(ASource.Pen);
  Brush.Assign(ASource.Brush);
  PolyFillMode := ASource.PolyFillMode;
  BackgroundMode := ASource.BackgroundMode;
  BackgroundColor := ASource.BackgroundColor;
  ViewPortOrg := ASource.ViewPortOrg;
  ViewPortExt := ASource.ViewPortExt;
  WindowExt := ASource.WindowExt;
  WindowOrg := ASource.WindowOrg;
  TextColor := ASource.TextColor;
  Transform := ASource.Transform;
  MapMode := ASource.MapMode;
end;

function TdxPSPDFMetaFileCanvasState.Clone: TdxPSPDFMetaFileCanvasState;
begin
  Result := TdxPSPDFMetaFileCanvasState.Create;
  Result.Assign(Self);
end;

{ TdxPSPDFMetaFileTextInfo }

constructor TdxPSPDFMetaFileTextInfo.Create;
begin
  inherited Create;
  FFont := TFont.Create;
end;

destructor TdxPSPDFMetaFileTextInfo.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TdxPSPDFMetaFileTextInfo.Concatenate(AInfo: TdxPSPDFMetaFileTextInfo): Boolean;
begin
  Result := (Options = AInfo.Options) and dxAreFontsEqual(Font, AInfo.Font) and
    (TextBounds.Top = AInfo.TextBounds.Top) and (TextBounds.Bottom = AInfo.TextBounds.Bottom) and
    (AInfo.TextBounds.Left - TextBounds.Right <= 1);

  if Result then
  begin
    Inc(FTextWidth, AInfo.TextWidth);
    FTextBounds.Right := AInfo.TextBounds.Right;
    FText := Text + AInfo.Text;
  end;
end;

procedure TdxPSPDFMetaFileTextInfo.Initialize(const R: PEMRExtTextOut; AIsUnicode: Boolean; AFont: TFont);

  function ExtractText(const R: PEMRExtTextOut; AIsUnicode: Boolean): string;
  var
    AFontData: TdxPSTTFFile;
    ATextA: AnsiString;
    I: Integer;
  begin
    if AIsUnicode then
      SetString(Result, PWideChar(TdxNativeUInt(R) + R^.emrtext.offString), R^.emrtext.nChars)
    else
    begin
      SetString(ATextA, PAnsiChar(TdxNativeUInt(R) + R^.emrtext.offString), R^.emrtext.nChars);
      Result := dxAnsiStringToString(ATextA, dxGetCodePageFromCharset(AFont.Charset));
    end;

    if Options and ETO_GLYPH_INDEX = ETO_GLYPH_INDEX then
    begin
      AFontData := TdxPSTTFFile.Create(AFont);
      try
        for I := 1 to Length(Result) do
          Result[I] := WideChar(AFontData.CharCodeByGlyphIndex[Word(Result[I])]);
      finally
        AFontData.Free;
      end;
    end;
  end;

begin
  FFont.Assign(AFont);
  FOptions := R^.emrtext.fOptions;

  FText := ExtractText(R, AIsUnicode);
  FTextOrigin := R^.emrtext.ptlReference;
  FTextWidth := CalculateTextWidth(R);
  FTextBounds := R^.rclBounds;

  if (TextBounds.Right < 0) and (TextBounds.Bottom < 0) then
    FTextBounds := Bounds(TextOrigin.X, TextOrigin.Y, TextWidth, Abs(AFont.Size))
  else
    if FFont.Orientation = 0 then
    begin
      if PtInRect(TextBounds, TextOrigin) then
        FTextOrigin := TextBounds.TopLeft
      else
        FTextBounds := cxRectSetOrigin(TextBounds, TextOrigin);

      FTextBounds.Bottom := TextBounds.Bottom + Abs(AFont.Size);
    end;
end;

function TdxPSPDFMetaFileTextInfo.CalculateTextWidth(const R: PEMRExtTextOut): Integer;
var
  AOffsets: PInteger;
  I: Integer;
begin
  Result := 0;
  if R^.emrtext.offDx > 0 then
  begin
    AOffsets := PInteger(TdxNativeUInt(R) + R^.emrtext.offDx);
    for I := 0 to R^.emrtext.nChars - 1 do
    begin
      Inc(Result, AOffsets^);
      Inc(AOffsets);
    end;
  end;
end;

function TdxPSPDFMetaFileTextInfo.GetNoClip: Boolean;
begin
  Result := Options and ETO_CLIPPED = 0;
end;

end.
