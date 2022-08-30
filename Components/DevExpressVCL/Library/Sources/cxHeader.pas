{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxHeader;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Controls, Graphics, ImgList, Messages, SysUtils, CommCtrl,
  dxCore, dxMessages, cxClasses, cxContainer, cxControls, cxGraphics, cxGeometry,
  cxLookAndFeelPainters, cxLookAndFeels,
  cxEdit, cxEditUtils, cxExtEditConsts, cxExtEditUtils;

type
  TcxHeaderSortOrder = TdxSortOrder;
  TcxCustomHeader = class;
  TcxHeader = class;
  TcxHeaderSection = class;
  TcxHeaderSections = class;
  TcxSectionTrackState = (tsTrackBegin, tsTrackMove, tsTrackEnd, tsTrackNone);
  TcxHeaderSectionRects = TRects;
  TcxHeaderSectionWidths = array of Integer;
  TcxHeaderState = (hsNone, hsSized, hsDragged, hsPressed);
  THeaderItemInfo = record
    ImageIndex: Integer;
    Rect: TRect;
    SectionAlignment: TAlignment;
    SortOrder: TcxHeaderSortOrder;
    State: TcxButtonState;
    Text: string;
    UseRightToLeftAlignment: Boolean;
  end;
  PHeaderItemInfo = ^THeaderItemInfo;

  TcxDrawSectionEvent = procedure(HeaderControl: TcxCustomHeader;
    Section: TcxHeaderSection; const ARect: TRect; Pressed: Boolean) of object;
  TcxSectionNotifyEvent = procedure(HeaderControl: TcxCustomHeader;
    Section: TcxHeaderSection) of object;
  TcxSectionTrackEvent = procedure(HeaderControl: TcxCustomHeader;
    Section: TcxHeaderSection; Width: Integer; State: TcxSectionTrackState) of object;
  TcxSectionDragEvent = procedure (Sender: TObject; FromSection, ToSection: TcxHeaderSection;
    var AllowDrag: Boolean) of object;
  TcxSectionChangingSortOrderEvent = procedure (Sender: TObject;
    const Section: TcxHeaderSection; const AOldSortOrder: TcxHeaderSortOrder;
    var ANewSortOrder: TcxHeaderSortOrder; var AllowChange: Boolean) of object;
  TcxSectionChangedSortOrderEvent = procedure (Sender: TObject;
    const Section: TcxHeaderSection; const ASortOrder: TcxHeaderSortOrder) of object;

  { DXM_GETHEADERITEMINFO }

  TDXMHeaderItemInfo = {$IFNDEF DELPHI16}packed{$ENDIF} record
    Msg: Cardinal;
    Index: TdxNativeInt;
    HeaderItemInfo: PHeaderItemInfo;
    Result: LRESULT;
  end;

  { TcxHeaderSection }

  TcxHeaderSection = class(TCollectionItem)
  strict private
    FAlignment: TAlignment;
    FAllowClick: Boolean;
    FAllowResize: Boolean;
    FAutoSize: Boolean;
    FBiDiMode: TBiDiMode;
    FDataIndex: Integer;
    FImageIndex: TcxImageIndex;
    FMaxWidth: TcxNaturalNumber;
    FMinWidth: TcxNaturalNumber;
    FParentBiDiMode: Boolean;
    FSortOrder: TcxHeaderSortOrder;
    FState: TcxButtonState;
    FText: string;
    FWidth: Integer;

    function GetLeft: Integer;
    function GetRight: Integer;
    function IsBiDiModeStored: Boolean;
    function IsDataIndexStored: Boolean;
    function GetHeaderControl: TcxCustomHeader;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSize(Value: Boolean);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetDataIndex(Value: Integer);
    procedure SetMaxWidth(Value: TcxNaturalNumber);
    procedure SetMinWidth(Value: TcxNaturalNumber);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetState(Value: TcxButtonState);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetImageIndex(const Value: TcxImageIndex);
    procedure SetSortOrder(Value: TcxHeaderSortOrder);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function GetDisplayName: string; override;
    property State: TcxButtonState read FState write SetState;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure ParentBiDiModeChanged;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;

    property Left: Integer read GetLeft;
    property Right: Integer read GetRight;
    property HeaderControl: TcxCustomHeader read GetHeaderControl;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AllowClick: Boolean read FAllowClick write FAllowClick default False;
    property AllowResize: Boolean read FAllowResize write FAllowResize default True;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property DataIndex: Integer read FDataIndex write SetDataIndex stored IsDataIndexStored;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property MaxWidth: TcxNaturalNumber read FMaxWidth write SetMaxWidth default 1000;
    property MinWidth: TcxNaturalNumber read FMinWidth write SetMinWidth default 30;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property SortOrder: TcxHeaderSortOrder read FSortOrder write SetSortOrder default soNone;
    property Text: string read FText write SetText;
    property Width: Integer read FWidth write SetWidth default 50;
  end;

  { TcxHeaderSections }

  TcxHeaderSections = class(TCollection)
  strict private
    function GetFirstSortedSection: TcxHeaderSection;
    function GetItem(Index: Integer): TcxHeaderSection;
    procedure SetItem(Index: Integer; Value: TcxHeaderSection);
  protected
    FHeaderControl: TcxCustomHeader;

    procedure ChangeScale(M, D: Integer); virtual;
    function GetOwner: TPersistent; override;
    function GetSortedSection(AIndexFrom: Integer): TcxHeaderSection;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(HeaderControl: TcxCustomHeader);
    procedure Assign(Source: TPersistent); override;
    function Add: TcxHeaderSection;
    function Insert(Index: Integer): TcxHeaderSection;
    property Items[Index: Integer]: TcxHeaderSection read GetItem write SetItem; default;
    property FirstSortedSection: TcxHeaderSection read GetFirstSortedSection;
  end;

  { TcxCustomHeader }

  TcxCustomHeader = class(TcxControl, IcxMouseTrackingCaller)
  private
    FAllowMultiSort: Boolean;
    FAllowSort: Boolean;
    FBrush: TBrush;
    FDragReorder: Boolean;
    FHotSectionIndex: Integer;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FLastDrawPosOnMove: Integer;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMouseOnSizer: Boolean;
    FPrevBrush: HBrush;
    FPrevMousePos: Integer;
    FResizeUpdate: Boolean;
    FScreenCursor: TCursor;
    FScreenCursorSaved: Boolean;
    FSectionDragged: Boolean;
    FSections: TcxHeaderSections;
    FSectionStream: TMemoryStream;
    FSelectedSectionIndex: Integer;
    FSizedSectionIndex: Integer;
    FState: TcxHeaderState;
    FUnderMouseSectionIndex: Integer;

    // Flags
    FSectionsFitCalculating: Boolean;
    FSortingChanging: Boolean;

    FOnSectionClick: TcxSectionNotifyEvent;
    FOnSectionResize: TcxSectionNotifyEvent;
    FOnSectionEndResize: TcxSectionNotifyEvent;
    FOnSectionTrack: TcxSectionTrackEvent;
    FOnDrawSection: TcxDrawSectionEvent;
    FOnSectionEndDrag: TNotifyEvent;
    FOnSectionDrag: TcxSectionDragEvent;
    FOnSectionChangingSortOrder: TcxSectionChangingSortOrderEvent;
    FOnSectionChangedSortOrder: TcxSectionChangedSortOrderEvent;
    FOnSectionChange: TNotifyEvent;
    FOnSectionsChange: TNotifyEvent;
    procedure ImageListChange(Sender: TObject);
    procedure RestoreScreenCursor;
    procedure SetScreenCursor(ACursor: TCursor);
    procedure SetSections(Value: TcxHeaderSections);
    procedure UpdateSection(Index: Integer);
    procedure UpdateSections;
    procedure SetImages(Value: TCustomImageList);
    procedure SetAllowMultiSort(Value: Boolean);
    procedure SetAllowSort(Value: Boolean);
    function GetSectionIndexAtPos(X, Y: Integer; AIncludeNonSectionPart: Boolean = False): Integer;

    function GetSelectedSection: TcxHeaderSection;
    function GetSizedSection: TcxHeaderSection;
    function GetUnderMouseSection: TcxHeaderSection;
    function IsSectionIndex(AIndex: Integer): Boolean;
    procedure IcxMouseTrackingCaller.MouseLeave = HeaderMouseLeave;
    procedure HeaderMouseLeave;
    procedure SetHotSectionIndex(Value: Integer);
    procedure SetMouseOnSizer(Value: Boolean);
    procedure SetSectionState(AIndex: Integer; AState: TcxButtonState);
    procedure SetSelectedSectionIndex(Value: Integer);
    procedure SetSizedSectionIndex(Value: Integer);
    procedure SetUnderMouseSectionIndex(Value: Integer);
    procedure UpdateDraggedSection;
    procedure UpdatePressedSection;
    procedure UpdateSizedSection;
    procedure UpdateUnderMouseSection;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure HDMGetItemCount(var Message: TMessage); message HDM_GETITEMCOUNT;
    procedure HDMGetItemInfo(var Message: TDXMHeaderItemInfo); message DXM_GETHEADERITEMINFO;

    procedure AllocateSplitLineDC;
    procedure ReleaseSplitLine;
    procedure DrawSplitLine(XPos: Integer);
    procedure InitResize(XPos: Integer);

    property HotSectionIndex: Integer read FHotSectionIndex write SetHotSectionIndex;
    property MouseOnSizer: Boolean read FMouseOnSizer write SetMouseOnSizer;
    property SelectedSection: TcxHeaderSection read GetSelectedSection;
    property SelectedSectionIndex: Integer read FSelectedSectionIndex write SetSelectedSectionIndex;
    property SizedSection: TcxHeaderSection read GetSizedSection;
    property SizedSectionIndex: Integer read FSizedSectionIndex write SetSizedSectionIndex;
    property UnderMouseSection: TcxHeaderSection read GetUnderMouseSection;
    property UnderMouseSectionIndex: Integer read FUnderMouseSectionIndex write SetUnderMouseSectionIndex;
  protected
    procedure BiDiModeChanged; override;
    procedure ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean); override;
    procedure ChangeSectionSortOrder(ASection: TcxHeaderSection;
      ANewSortOrder: TcxHeaderSortOrder; ADeleteOtherSorting: Boolean); dynamic;
    function DoSectionChangingSortOrder(ASection: TcxHeaderSection;
      var ANewSortOrder: TcxHeaderSortOrder): Boolean;
    procedure DoSectionChangedSortOrder(ASection: TcxHeaderSection);
    function DoSectionDrag(FromSection, ToSection: TcxHeaderSection): Boolean; virtual;
    procedure DoSectionEndDrag; virtual;
    function GetSectionRect(Index: Integer): TRect;
    function GetSectionRectBySectionWidths(AHeaderWidth: Integer;
      const ASectionWidths: TcxHeaderSectionWidths; AIndex: Integer): TRect; virtual;
    function CreateSection: TcxHeaderSection; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DrawSection(SectionIndex: Integer); dynamic;
    procedure DoSectionClickEvent(Section: TcxHeaderSection); dynamic;
    procedure DoSectionChange;
    procedure DoSectionsChange;
    procedure DoSectionDragEvent(FromSection, ToSection: TcxHeaderSection; var AllowDrag: Boolean); dynamic;
    procedure DoSectionEndDragEvent; dynamic;
    procedure DoSectionResizeEvent(Section: TcxHeaderSection); dynamic;
    procedure DoSectionEndResizeEvent(Section: TcxHeaderSection); dynamic;
    procedure DoSectionTrackEvent(Section: TcxHeaderSection; Width: Integer;
      State: TcxSectionTrackState); dynamic;
    procedure DisableSort(AExceptSection: TcxHeaderSection = nil); virtual;
    procedure FitToClientWidth;
    function GetBackgroundColor: TColor; virtual;
    function GetSectionContentBounds(ASectionBounds: TRect; AState: TcxButtonState): TRect;
    function GetSplitLineOffset: Integer; virtual;
    function GetTextColor: TColor; virtual;
    function IsInnerControl: Boolean; virtual;
    procedure Paint; override;

    property OnSectionsChange: TNotifyEvent read FOnSectionsChange write FOnSectionsChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CalcSectionWidths(AHeaderWidth: Integer; out AWidths: TcxHeaderSectionWidths); virtual;
    procedure FlipChildren(AllLevels: Boolean); override;
    function GetAutoHeight: Integer;
    property AllowMultiSort: Boolean read FAllowMultiSort write SetAllowMultiSort default True;
    property AllowSort: Boolean read FAllowSort write SetAllowSort default True;
    property DragReorder: Boolean read FDragReorder write FDragReorder default False;
    property Images: TCustomImageList read FImages write SetImages;
    property LookAndFeel;
    property ResizeUpdate: Boolean read FResizeUpdate write FResizeUpdate default True;
    property Sections: TcxHeaderSections read FSections write SetSections;
    property OnDrawSection: TcxDrawSectionEvent read FOnDrawSection
      write FOnDrawSection;
    property OnSectionChange: TNotifyEvent read FOnSectionChange
      write FOnSectionChange;
    property OnSectionChangedSortOrder: TcxSectionChangedSortOrderEvent
      read FOnSectionChangedSortOrder write FOnSectionChangedSortOrder;
    property OnSectionChangingSortOrder: TcxSectionChangingSortOrderEvent
      read FOnSectionChangingSortOrder write FOnSectionChangingSortOrder;
    property OnSectionClick: TcxSectionNotifyEvent read FOnSectionClick
      write FOnSectionClick;
    property OnSectionDrag: TcxSectionDragEvent read FOnSectionDrag
      write FOnSectionDrag;
    property OnSectionEndDrag: TNotifyEvent read FOnSectionEndDrag
      write FOnSectionEndDrag;
    property OnSectionEndResize: TcxSectionNotifyEvent read FOnSectionEndResize
      write FOnSectionEndResize;
    property OnSectionResize: TcxSectionNotifyEvent read FOnSectionResize
      write FOnSectionResize;
    property OnSectionTrack: TcxSectionTrackEvent read FOnSectionTrack
      write FOnSectionTrack;
  end;

  { TcxHeader }

  TcxHeader = class(TcxCustomHeader)
  published
    property Align;
    property AllowSort;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DragReorder;
    property Enabled;
    property Font;
    property Images;
    property LookAndFeel;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ResizeUpdate;
    property Sections;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawSection;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSectionChange;
    property OnSectionChangedSortOrder;
    property OnSectionChangingSortOrder;
    property OnSectionClick;
    property OnSectionDrag;
    property OnSectionEndDrag;
    property OnSectionEndResize;
    property OnSectionResize;
    property OnSectionTrack;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure DrawScaledHeaderSection(AHeaderHandle: HWND; AIndex: Integer; ACanvas: TcxCanvas; ALookAndFeel: TcxLookAndFeel;
  AScaleFactor: TdxScaleFactor; AImages: TCustomImageList = nil);
procedure DrawHeaderSection(AHeaderHandle: HWND; AIndex: Integer; ACanvas: TcxCanvas;
  ALookAndFeel: TcxLookAndFeel; AImages: TCustomImageList = nil);

implementation

uses
  Forms, Types, Math, dxThemeConsts, dxThemeManager, dxUxTheme, dxDPIAwareUtils;

const
  cxHeaderSectionTextLeftRightIndent = 4;
  cxHeaderSectionTextTopBottomIndent = 2;

procedure DrawHeaderSection(AHeaderHandle: HWND; AIndex: Integer;
  ACanvas: TcxCanvas; ALookAndFeel: TcxLookAndFeel; AImages: TCustomImageList);
begin
  DrawScaledHeaderSection(AHeaderHandle, AIndex, ACanvas, ALookAndFeel, dxSystemScaleFactor, AImages);
end;

procedure DrawScaledHeaderSection(AHeaderHandle: HWND; AIndex: Integer; ACanvas: TcxCanvas;
  ALookAndFeel: TcxLookAndFeel; AScaleFactor: TdxScaleFactor; AImages: TCustomImageList = nil);
const
  ImageTextOffset = 2;
  SortingMarkOffset = 4;

  procedure DrawSortingMark(const ARect: TRect; ASortOrder: TcxHeaderSortOrder; AUserRightToLeftAlignment: Boolean);
  var
    ASavedPenColor: TColor;
    ASortingMarkRect: TRect;
  begin
    if ASortOrder <> soNone then
    begin
      ASavedPenColor := ACanvas.Pen.Color;
      ASortingMarkRect := ARect;
      Dec(ASortingMarkRect.Right, AScaleFactor.Apply(SortingMarkOffset));
      ASortingMarkRect.Left := ASortingMarkRect.Right - ALookAndFeel.Painter.ScaledSortingMarkSize(AScaleFactor).X;
      if AUserRightToLeftAlignment then
        ASortingMarkRect := TdxRightToLeftLayoutConverter.ConvertRect(ASortingMarkRect, ARect);
      ALookAndFeel.Painter.DrawScaledSortingMark(ACanvas, ASortingMarkRect, ASortOrder = soAscending, AScaleFactor);
      ACanvas.Pen.Color := ASavedPenColor;
    end;
  end;

  function GetTextRect(const ANormalContentRect: TRect; ASortOrder: TcxHeaderSortOrder): TRect;
  begin
    Result := ANormalContentRect;
    Inc(Result.Left, AScaleFactor.Apply(cxHeaderSectionTextLeftRightIndent));
    Dec(Result.Right, AScaleFactor.Apply(cxHeaderSectionTextLeftRightIndent));
    if ASortOrder <> soNone then
      Dec(Result.Right, AScaleFactor.Apply(SortingMarkOffset) + ALookAndFeel.Painter.ScaledSortingMarkSize(AScaleFactor).X);
  end;

  function GetImageRect(var ATextRect: TRect; var AHeaderItemInfo: THeaderItemInfo): TRect;
  var
    AImageSize: TSize;
    ATextWidth: Integer;
    AImageRectLeft: Integer;
    AImageTextOffset: Integer;
  begin
    AImageSize := dxGetImageSize(AImages, AScaleFactor);
    ATextWidth := ACanvas.TextWidth(AHeaderItemInfo.Text);
    AImageTextOffset := AScaleFactor.Apply(ImageTextOffset);
    Result := cxRectSetWidth(ATextRect, AImageSize.cx);
    case AHeaderItemInfo.SectionAlignment of
      taLeftJustify:
        AImageRectLeft := ATextRect.Left;
      taRightJustify:
        AImageRectLeft := ATextRect.Right - (ATextWidth + AImageSize.cx + AImageTextOffset);
    else {taCenter:}
      AImageRectLeft := ATextRect.Left + (cxRectWidth(ATextRect) - AImageSize.cx - AImageTextOffset - ATextWidth) div 2;
    end;
    Result := cxRectSetLeft(cxRectSetWidth(ATextRect, AImageSize.cx), AImageRectLeft);
    if Result.Left < ATextRect.Left then
    begin
      Result := cxRectSetLeft(Result, ATextRect.Left);
      AHeaderItemInfo.SectionAlignment := taLeftJustify
    end;
    Inc(ATextRect.Left, AImageSize.cx + AImageTextOffset);
  end;

var
  AHeaderItemInfo: THeaderItemInfo;
  AHeaderRect: TRect;
  AItemCount: Integer;
  ANeighbors: TcxNeighbors;
  ANormalContentRect, ATextRect, AImageRect: TRect;
  ATextAlignment: TAlignment;
begin
  AItemCount := Header_GetItemCount(AHeaderHandle);
  AHeaderRect := cxGetWindowRect(AHeaderHandle);
  AHeaderRect := Rect(0, 0, cxRectWidth(AHeaderRect), cxRectHeight(AHeaderRect));
  if AItemCount = 0 then
  begin
    ACanvas.FillRect(AHeaderRect);
    Exit;
  end;

  cxSendStructMessage(AHeaderHandle, DXM_GETHEADERITEMINFO, AIndex, AHeaderItemInfo);

  if AIndex = AItemCount then
    if AHeaderItemInfo.UseRightToLeftAlignment then
      AHeaderItemInfo.Rect.Left := AHeaderRect.Left - 10
    else
      AHeaderItemInfo.Rect.Right := AHeaderRect.Right + 10
  else
    if ALookAndFeel.Painter.LookAndFeelStyle = lfsUltraFlat then
      if AHeaderItemInfo.UseRightToLeftAlignment then
        Dec(AHeaderItemInfo.Rect.Left)
      else
        Inc(AHeaderItemInfo.Rect.Right);

  ANormalContentRect := ALookAndFeel.Painter.HeaderControlSectionContentBounds(AHeaderItemInfo.Rect, cxbsNormal);
  ATextRect := GetTextRect(ANormalContentRect, AHeaderItemInfo.SortOrder);

  if Assigned(AImages) and (AHeaderItemInfo.ImageIndex >= 0) then
    AImageRect := GetImageRect(ATextRect, AHeaderItemInfo);

  ATextAlignment := AHeaderItemInfo.SectionAlignment;
  if AHeaderItemInfo.UseRightToLeftAlignment then
  begin
    AImageRect := TdxRightToLeftLayoutConverter.ConvertRect(AImageRect, AHeaderItemInfo.Rect);
    ATextRect := TdxRightToLeftLayoutConverter.ConvertRect(ATextRect, AHeaderItemInfo.Rect);
    ChangeBiDiModeAlignment(ATextAlignment);
  end;

  ACanvas.SaveState;
  try
    ANeighbors := [];
    if AIndex > 0 then
      if AHeaderItemInfo.UseRightToLeftAlignment then
        Include(ANeighbors, nRight)
      else
        Include(ANeighbors, nLeft);
    if AIndex < AItemCount then
      if AHeaderItemInfo.UseRightToLeftAlignment then
        Include(ANeighbors, nLeft)
      else
        Include(ANeighbors, nRight);

    ACanvas.IntersectClipRect(AHeaderItemInfo.Rect);
    ALookAndFeel.Painter.DrawScaledHeaderControlSection(ACanvas, AHeaderItemInfo.Rect, ATextRect, ANeighbors,
      cxBordersAll, AHeaderItemInfo.State, ATextAlignment, vaCenter, False, True,
      AHeaderItemInfo.Text, ACanvas.Font, ACanvas.Font.Color, ACanvas.Brush.Color, AScaleFactor);

    ACanvas.IntersectClipRect(ANormalContentRect);
    if IsImageAssigned(AImages, AHeaderItemInfo.ImageIndex) then
      cxDrawImage(ACanvas, AImageRect, nil, AImages, AHeaderItemInfo.ImageIndex, ifmNormal, idmNormal, False, nil, AScaleFactor);

    DrawSortingMark(AHeaderItemInfo.Rect, AHeaderItemInfo.SortOrder, AHeaderItemInfo.UseRightToLeftAlignment);
  finally
    ACanvas.RestoreState;
  end;
end;

function GetNextSortOrder(ASortOrder: TcxHeaderSortOrder; ADeleteSorting: Boolean): TcxHeaderSortOrder;
begin
  if ADeleteSorting then
    Result := soNone
  else
    if ASortOrder = soAscending then
      Result := soDescending
    else
      Result := soAscending;
end;

{ TcxCustomHeader }

constructor TcxCustomHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [];
  DoubleBuffered := True;
  Height := 17;
  Width := 117;

  FAllowSort := True;
  FResizeUpdate := True;
  FSectionStream := nil;
  FSelectedSectionIndex := -1;
  FSizedSectionIndex := -1;
  FUnderMouseSectionIndex := -1;
  FHotSectionIndex := -1;

  FSections := TcxHeaderSections.Create(Self);
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChange;

  FAllowMultiSort := True;
end;

destructor TcxCustomHeader.Destroy;
begin
  EndMouseTracking(Self);
  FreeAndNil(FSections);
  FreeAndNil(FImagesChangeLink);
  FreeAndNil(FSectionStream);
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure TcxCustomHeader.CalcSectionWidths(AHeaderWidth: Integer;
  out AWidths: TcxHeaderSectionWidths);
var
  ANewSectionWidth, APrevNonFixedWidthSectionCount, ARemainedWidth: Integer;
  ASectionWidth, ATempWidth, AWorkWidth, I: Integer;
  ANonFixedWidthSections: TList;
  ASection: TcxHeaderSection;
begin
  ANonFixedWidthSections := TList.Create;
  try
    SetLength(AWidths, Sections.Count);
    AWorkWidth := AHeaderWidth;
    for I := 0 to Sections.Count - 1 do
    begin
      ASection := Sections[I];
      if ASection.AutoSize then
        ANonFixedWidthSections.Add(ASection)
      else
      begin
        Dec(AWorkWidth, ASection.Width);
        AWidths[I] := ASection.Width;
      end;
    end;
    if ANonFixedWidthSections.Count > 0 then
      repeat
        APrevNonFixedWidthSectionCount := ANonFixedWidthSections.Count;
        ATempWidth := AWorkWidth div APrevNonFixedWidthSectionCount;
        ARemainedWidth := AWorkWidth mod APrevNonFixedWidthSectionCount;
        for I := APrevNonFixedWidthSectionCount - 1 downto 0 do
        begin
          ASection := TcxHeaderSection(ANonFixedWidthSections[I]);
          ASectionWidth := ATempWidth;
          if I = APrevNonFixedWidthSectionCount - 1 then
            Inc(ASectionWidth, ARemainedWidth);
          ANewSectionWidth := Max(ASectionWidth, ASection.MinWidth);
          ANewSectionWidth := Min(ANewSectionWidth, ASection.MaxWidth);
          AWidths[ASection.Index] := ANewSectionWidth;
          if ASectionWidth <> ANewSectionWidth then
          begin
            ANonFixedWidthSections.Remove(ASection);
            Dec(AWorkWidth, ANewSectionWidth);
          end;
        end;
      until (ANonFixedWidthSections.Count = 0) or (ANonFixedWidthSections.Count = APrevNonFixedWidthSectionCount);
  finally
    ANonFixedWidthSections.Free;
  end;
end;

procedure TcxCustomHeader.FlipChildren(AllLevels: Boolean);
var
  I, AFirstSectionWidth, ALastSectionWidth: Integer;
  ASectionList: TcxHeaderSections;
begin
  if HandleAllocated and (Sections.Count > 0) then
  begin
    ALastSectionWidth := ClientWidth;
    AFirstSectionWidth := Sections[0].Width;
    for I := 0 to Sections.Count - 2 do
      Dec(ALastSectionWidth, Sections[I].Width);

    ASectionList := TcxHeaderSections.Create(Self);
    try
      for I := 0 to Sections.Count - 1 do
        with ASectionList.Add do
          Assign(Sections[I]);
      for I := 0 to Sections.Count - 1 do
        Sections[I].Assign(ASectionList[Sections.Count - I - 1]);
    finally
      ASectionList.Free;
    end;

    if Sections.Count > 1 then
    begin
      Sections[Sections.Count - 1].Width := AFirstSectionWidth;
      Sections[0].Width := ALastSectionWidth;
    end;
    UpdateSections;
  end;
end;

function TcxCustomHeader.GetAutoHeight: Integer;
begin
  Canvas.Font.Assign(Font);
  Canvas.Font.Style := [fsBold, fsUnderline];
  Result := Max(cxTextHeight(Canvas.Handle), dxGetImageSize(Images, ScaleFactor).cy) +
    2 * (LookAndFeel.Painter.HeaderControlSectionBorderSize(cxbsNormal) + ScaleFactor.Apply(cxHeaderSectionTextTopBottomIndent));
end;

procedure TcxCustomHeader.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and not(CS_HREDRAW or CS_VREDRAW);
end;

function TcxCustomHeader.CreateSection: TcxHeaderSection;
begin
  Result := TcxHeaderSection.Create(Sections);
end;

procedure TcxCustomHeader.CreateWnd;

  procedure ReadSections;
  var
    AReader: TReader;
  begin
    Sections.BeginUpdate;
    try
      Sections.Clear;
      AReader := TReader.Create(FSectionStream, 1024);
      try
        AReader.ReadValue;
        AReader.ReadCollection(Sections);
      finally
        AReader.Free;
      end;
      FreeAndNil(FSectionStream);
    finally
      Sections.EndUpdate;
    end;
  end;

begin
  inherited CreateWnd;
  if (FSections.Count = 0) and (FSectionStream <> nil) then
    ReadSections
  else
    UpdateSections;
  BiDiModeChanged;
end;

procedure TcxCustomHeader.DestroyWnd;
var
  AWriter: TWriter;
begin
  if FSectionStream = nil then
    FSectionStream := TMemoryStream.Create;
  AWriter := TWriter.Create(FSectionStream, 1024);
  try
    AWriter.WriteCollection(FSections);
  finally
    AWriter.Free;
    FSectionStream.Position := 0;
  end;
  inherited DestroyWnd;
end;

procedure TcxCustomHeader.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  Invalidate;
end;

procedure TcxCustomHeader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TcxCustomHeader.SetSections(Value: TcxHeaderSections);
begin
  FSections.Assign(Value);
end;

procedure TcxCustomHeader.UpdateSection(Index: Integer);
begin
  InvalidateRect(GetSectionRect(Index), False);
end;

procedure TcxCustomHeader.UpdateSections;
begin
  InvalidateRect(ClientRect, False);
end;

procedure TcxCustomHeader.AllocateSplitLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
  if not ResizeUpdate then
  begin
    if not Assigned(FBrush) then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TcxCustomHeader.BiDiModeChanged;
var
  I: Integer;
begin
  inherited;
  if HandleAllocated then
    for I := 0 to Sections.Count - 1 do
      if Sections[I].ParentBiDiMode then
        Sections[I].ParentBiDiModeChanged;
end;

procedure TcxCustomHeader.DrawSplitLine(XPos: Integer);
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  FLastDrawPosOnMove := XPos;
  P := Point(XPos - GetSplitLineOffset, 0);
  P := dxMapWindowPoint(Handle, Parent.Handle, P);
  PatBlt(FLineDC, P.X, 1, 1, Parent.Height - 1, DSTINVERT);
end;

procedure TcxCustomHeader.ReleaseSplitLine;
begin
  if FLineVisible then
    DrawSplitLine(FLastDrawPosOnMove);
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  FreeAndNil(FBrush);
end;

procedure TcxCustomHeader.InitResize(XPos: Integer);
begin
  FLineVisible := False;
  AllocateSplitLineDC;
  if not ResizeUpdate then
    DrawSplitLine(XPos);
end;

procedure TcxCustomHeader.ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean);
begin
  inherited;
  Sections.ChangeScale(M, D);
end;

procedure TcxCustomHeader.ChangeSectionSortOrder(ASection: TcxHeaderSection;
  ANewSortOrder: TcxHeaderSortOrder; ADeleteOtherSorting: Boolean);
begin
  if not AllowSort or (ANewSortOrder = ASection.SortOrder) then
    Exit;

  if DoSectionChangingSortOrder(ASection, ANewSortOrder) then
  begin
    if ADeleteOtherSorting or not FAllowMultiSort then
      DisableSort(ASection);

    FSortingChanging := True;
    try
      ASection.SortOrder := ANewSortOrder;
    finally
      FSortingChanging := False;
    end;
    DoSectionChangedSortOrder(ASection);
  end;
end;

function TcxCustomHeader.DoSectionChangingSortOrder(ASection: TcxHeaderSection; var ANewSortOrder: TcxHeaderSortOrder): Boolean;
begin
  Result := True;
  if Assigned(FOnSectionChangingSortOrder) then
    FOnSectionChangingSortOrder(Self, ASection, ASection.SortOrder,  ANewSortOrder, Result);
end;

procedure TcxCustomHeader.DoSectionChangedSortOrder(ASection: TcxHeaderSection);
begin
  if Assigned(FOnSectionChangedSortOrder) then
    FOnSectionChangedSortOrder(Self, ASection, ASection.SortOrder);
end;

function TcxCustomHeader.DoSectionDrag(FromSection, ToSection: TcxHeaderSection): Boolean;
begin
  Result := True;
  DoSectionDragEvent(FromSection, ToSection, Result);
end;

procedure TcxCustomHeader.DoSectionEndDrag;
var
  AIndexFrom: Integer;
begin
  if UnderMouseSectionIndex <> -1 then
  begin
    AIndexFrom := SelectedSectionIndex;
    SelectedSectionIndex := -1;
    HotSectionIndex := -1;
    Sections[AIndexFrom].Index := UnderMouseSectionIndex;
    HotSectionIndex := UnderMouseSectionIndex;
  end;
  DoSectionEndDragEvent;
end;

procedure TcxCustomHeader.DrawSection(SectionIndex: Integer);
begin
  if IsSectionIndex(SectionIndex) and Assigned(FOnDrawSection) then
    FOnDrawSection(Self, FSections[SectionIndex], GetSectionRect(SectionIndex), FSections[SectionIndex].State = cxbsPressed)
  else
    DrawScaledHeaderSection(Handle, SectionIndex, Canvas, LookAndFeel, ScaleFactor, Images);
end;

procedure TcxCustomHeader.ImageListChange(Sender: TObject);
begin
  UpdateSections;
end;

procedure TcxCustomHeader.RestoreScreenCursor;
begin
  if FScreenCursorSaved then
  begin
    FScreenCursorSaved := False;
    Screen.Cursor := FScreenCursor;
  end;
end;

procedure TcxCustomHeader.SetScreenCursor(ACursor: TCursor);
begin
  if not FScreenCursorSaved then
  begin
    FScreenCursor := Screen.Cursor;
    FScreenCursorSaved := True;
  end;
  Screen.Cursor := ACursor;
end;

procedure TcxCustomHeader.WMSize(var Message: TWMSize);
begin
  inherited;
  FitToClientWidth;
end;

procedure TcxCustomHeader.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Invalidate;
end;

procedure TcxCustomHeader.HDMGetItemCount(var Message: TMessage);
begin
  Message.Result := Sections.Count;
end;

procedure TcxCustomHeader.HDMGetItemInfo(var Message: TDXMHeaderItemInfo);

  function GetState(AIndex: Integer): TcxButtonState;
  begin
    if not Enabled then
      Result := cxbsDisabled
    else
      if IsSectionIndex(AIndex) then
        Result := FSections[AIndex].State
      else
        Result := cxbsNormal;
  end;

var
  AIndex: Integer;
  AHeaderItemInfo: PHeaderItemInfo;
  AAlignment: TAlignment;
begin
  AIndex := Message.Index;
  AHeaderItemInfo := Message.HeaderItemInfo;
  ZeroMemory(AHeaderItemInfo, SizeOf(THeaderItemInfo));
  AHeaderItemInfo.ImageIndex := -1;
  AHeaderItemInfo.State := GetState(AIndex);
  AHeaderItemInfo.Rect := GetSectionRect(AIndex);
  if IsSectionIndex(AIndex) then
  begin
    AHeaderItemInfo.Text := FSections[AIndex].Text;
    AAlignment := FSections[AIndex].Alignment;
    AHeaderItemInfo.SectionAlignment := AAlignment;
    AHeaderItemInfo.ImageIndex := FSections[AIndex].ImageIndex;
    AHeaderItemInfo.SortOrder := FSections[AIndex].SortOrder;
  end;
  AHeaderItemInfo.UseRightToLeftAlignment := UseRightToLeftAlignment;
  Message.HeaderItemInfo := AHeaderItemInfo;
end;

procedure TcxCustomHeader.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImagesChangeLink, Self);
end;

procedure TcxCustomHeader.DoSectionClickEvent(Section: TcxHeaderSection);
begin
  if Assigned(FOnSectionClick) then FOnSectionClick(Self, Section);
end;

procedure TcxCustomHeader.DoSectionChange;
begin
  if Assigned(OnSectionChange) then
    OnSectionChange(Self);
end;

procedure TcxCustomHeader.DoSectionsChange;
begin
  if Assigned(OnSectionsChange) then
    OnSectionsChange(Self);
end;

procedure TcxCustomHeader.DoSectionDragEvent(FromSection, ToSection: TcxHeaderSection; var AllowDrag: Boolean);
begin
  if Assigned(FOnSectionDrag) then
    FOnSectionDrag(Self, FromSection, ToSection, AllowDrag);
  FSectionDragged := AllowDrag;
end;

procedure TcxCustomHeader.DoSectionEndDragEvent;
begin
  if Assigned(FOnSectionEndDrag) then FOnSectionEndDrag(Self);
end;

procedure TcxCustomHeader.DoSectionResizeEvent(Section: TcxHeaderSection);
begin
  if Assigned(FOnSectionResize) then FOnSectionResize(Self, Section);
end;

procedure TcxCustomHeader.DoSectionEndResizeEvent(Section: TcxHeaderSection);
begin
  if Assigned(FOnSectionEndResize) then
    FOnSectionEndResize(Self, Section);
  if not FSectionsFitCalculating then
    FitToClientWidth;
end;

procedure TcxCustomHeader.DoSectionTrackEvent(Section: TcxHeaderSection; Width: Integer; State: TcxSectionTrackState);
begin
  if Assigned(FOnSectionTrack) then FOnSectionTrack(Self, Section, Width, State);
end;

procedure TcxCustomHeader.Paint;
var
  I: Integer;
begin
  inherited Paint;

  Canvas.Font.Assign(Font);
  Canvas.Font.Color := GetTextColor;
  Canvas.Brush.Color := GetBackgroundColor;

  if (FSections.Count = 0) and not IsInnerControl and IsDesigning then
  begin
    Canvas.Pen.Style := psDash;
    Windows.Rectangle(Canvas.Handle, ClientRect.Left, ClientRect.Top,
      ClientRect.Right, ClientRect.Bottom);
  end
  else
    for I := 0 to FSections.Count do
      DrawSection(I);
end;

function TcxCustomHeader.GetSectionRect(Index: Integer): TRect;
var
  ASectionWidths: TcxHeaderSectionWidths;
  I: Integer;
begin
  SetLength(ASectionWidths, Sections.Count);
  for I := 0 to Sections.Count - 1 do
    ASectionWidths[I] := Sections[I].Width;
  Result := GetSectionRectBySectionWidths(Width, ASectionWidths, Index);
end;

function TcxCustomHeader.GetSectionRectBySectionWidths(AHeaderWidth: Integer;
  const ASectionWidths: TcxHeaderSectionWidths; AIndex: Integer): TRect;
var
  I: Integer;
begin
  Result := cxEmptyRect;
  if (AIndex >= 0) and (AIndex <= Sections.Count) then
  begin
    for I := 0 to AIndex - 1 do
      Inc(Result.Left, ASectionWidths[I]);
    if AIndex = Sections.Count then
      Result.Right := AHeaderWidth
    else
      Result.Right := Result.Left + ASectionWidths[AIndex];
    Result.Bottom := Height;
    if UseRightToLeftAlignment then
      Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, cxRectBounds(0, 0, AHeaderWidth, Height));
  end;
end;

procedure TcxCustomHeader.WMCaptureChanged(var Message: TMessage);
begin
  if (FState = hsNone) then
    Exit;

  case FState of
    hsSized:
      begin
        if not ResizeUpdate then
          ReleaseSplitLine;
        UpdateSizedSection;
      end;
    hsDragged:
      begin
        DoSectionEndDragEvent;
        RestoreScreenCursor;
      end;
  end;
  FState := hsNone;
  SelectedSectionIndex := -1;
end;

procedure TcxCustomHeader.WMLButtonDown(var Message: TWMLButtonDown);
var
  APos: Integer;
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;

  MouseCapture := True;
  if MouseOnSizer then
  begin
    FState := hsSized;
    if not ResizeUpdate then
     begin
      if UseRightToLeftAlignment then
        APos := SizedSection.Left
      else
        APos := SizedSection.Right;
      InitResize(APos);
     end;
    DoSectionTrackEvent(SizedSection, SizedSection.Width, tsTrackBegin);
    FPrevMousePos := Message.XPos;
  end
  else
  begin
    if (UnderMouseSectionIndex <> -1) and UnderMouseSection.AllowClick then
    begin
      FState := hsPressed;
      HotSectionIndex := -1;
      SelectedSectionIndex := UnderMouseSectionIndex;
    end;
  end;
end;

procedure TcxCustomHeader.WMMouseMove(var Message: TWMMouseMove);
var
  ACalcNewWidth: Integer;
  APos: Integer;
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;

  UpdateUnderMouseSection;

  case FState of
    hsSized:
      begin
        if UseRightToLeftAlignment then
          ACalcNewWidth := SizedSection.Width - Message.XPos + FPrevMousePos
        else
          ACalcNewWidth := SizedSection.Width + Message.XPos - FPrevMousePos;
        if ACalcNewWidth < SizedSection.MinWidth then
          ACalcNewWidth := SizedSection.MinWidth
        else
          if ACalcNewWidth > SizedSection.MaxWidth then
            ACalcNewWidth := SizedSection.MaxWidth;

        if not ResizeUpdate then
        begin
          if UseRightToLeftAlignment then
            APos := SizedSection.Right - ACalcNewWidth
          else
            APos := SizedSection.Left + ACalcNewWidth;
          if FLastDrawPosOnMove <> APos then
          begin
            DrawSplitLine(FLastDrawPosOnMove);
            DrawSplitLine(APos);
          end;
        end
        else
        begin
          if UseRightToLeftAlignment then
            FPrevMousePos := FPrevMousePos - ACalcNewWidth + SizedSection.Width
          else
            FPrevMousePos := FPrevMousePos + ACalcNewWidth - SizedSection.Width;
          SizedSection.Width := ACalcNewWidth;
          DoSectionTrackEvent(SizedSection, SizedSection.Width, tsTrackMove);
        end;

        DoSectionResizeEvent(SizedSection);
      end;
    hsPressed:
      begin
        if FDragReorder then
        begin
          FSectionDragged := False;
          FState := hsDragged;
          UpdateDraggedSection;
        end
        else
          UpdatePressedSection;
      end;
    hsDragged:
      begin
        UpdateDraggedSection;
        if SelectedSectionIndex <> UnderMouseSectionIndex then
          HotSectionIndex := UnderMouseSectionIndex
        else
          HotSectionIndex := -1;
      end;
    hsNone:
      begin
        UpdateSizedSection;
        if MouseOnSizer then
          HotSectionIndex := -1
        else
          HotSectionIndex := UnderMouseSectionIndex;
      end;
  end;

  BeginMouseTracking(Self, ClientRect, Self);
end;

procedure TcxCustomHeader.WMLButtonUp(var Message: TWMLButtonUp);
var
  ASection: TcxHeaderSection;
  AState: TShiftState;
begin
  inherited;
  if IsDesigning then
    Exit;

  case FState of
    hsSized:
      begin
        if not ResizeUpdate then
        begin
          if UseRightToLeftAlignment then
            SizedSection.Width := SizedSection.Right - FLastDrawPosOnMove
          else
            SizedSection.Width := FLastDrawPosOnMove - SizedSection.Left;
          ReleaseSplitLine;
        end;
        DoSectionTrackEvent(SizedSection, SizedSection.Width, tsTrackEnd);
        UpdateSizedSection;
      end;
    hsDragged:
      begin
        if FSectionDragged then
          DoSectionEndDrag;
        RestoreScreenCursor;
        UpdateSections;
      end;
    hsPressed:
      begin
        if UnderMouseSectionIndex = SelectedSectionIndex then
        begin
          AState := KeysToShiftState(Message.Keys);
          ASection := SelectedSection;
          DoSectionClickEvent(ASection);
          ChangeSectionSortOrder(ASection,
            GetNextSortOrder(ASection.SortOrder, ssCtrl in AState),
            [ssCtrl, ssShift] * AState = []);
        end;
      end;
  end;
  FState := hsNone;
  MouseCapture := False;
  SelectedSectionIndex := -1;
end;

function TcxCustomHeader.GetSectionIndexAtPos(X, Y: Integer; AIncludeNonSectionPart: Boolean): Integer;
var
  I: Integer;
  ACount: Integer;
begin
  Result := -1;
  if AIncludeNonSectionPart then
    ACount := FSections.Count
  else
    ACount := FSections.Count - 1;
  for I := 0 to ACount do
    if PtInRect(GetSectionRect(I), Point(X, Y)) and PtInRect(ClientRect, Point(X, Y)) then
    begin
      Result := I;
      Break;
    end;
end;

function TcxCustomHeader.GetSelectedSection: TcxHeaderSection;
begin
  Result := FSections[SelectedSectionIndex];
end;

function TcxCustomHeader.GetSizedSection: TcxHeaderSection;
begin
  Result := FSections[SizedSectionIndex];
end;

function TcxCustomHeader.GetUnderMouseSection: TcxHeaderSection;
begin
  Result := FSections[UnderMouseSectionIndex];
end;

function TcxCustomHeader.IsSectionIndex(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FSections.Count);
end;

procedure TcxCustomHeader.HeaderMouseLeave;
begin
  Perform(CM_MOUSELEAVE, 0, 0);
end;

procedure TcxCustomHeader.SetHotSectionIndex(Value: Integer);
begin
  if (FHotSectionIndex <> Value) then
  begin
    SetSectionState(FHotSectionIndex, cxbsNormal);
    FHotSectionIndex := Value;
    SetSectionState(FHotSectionIndex, cxbsHot);
  end;
end;

procedure TcxCustomHeader.SetMouseOnSizer(Value: Boolean);
begin
  if FMouseOnSizer <> Value then
  begin
    FMouseOnSizer := Value;
    if FMouseOnSizer then
      SetScreenCursor(crHSplit)
    else
      RestoreScreenCursor;
  end;
end;

procedure TcxCustomHeader.SetSectionState(AIndex: Integer; AState: TcxButtonState);
begin
  if (Sections.Count > 0) and (AIndex >= 0) and (AIndex < Sections.Count) then
    FSections[AIndex].State := AState;
end;

procedure TcxCustomHeader.SetSelectedSectionIndex(Value: Integer);
begin
  if Value <> -1 then
    SetSectionState(Value, cxbsPressed)
  else
    SetSectionState(FSelectedSectionIndex, cxbsNormal);
  FSelectedSectionIndex := Value;
end;

procedure TcxCustomHeader.SetSizedSectionIndex(Value: Integer);
begin
  FSizedSectionIndex := Value;
  if FSizedSectionIndex = -1 then
    MouseOnSizer := False
  else
    MouseOnSizer := True;
end;

procedure TcxCustomHeader.SetUnderMouseSectionIndex(Value: Integer);
begin
  FUnderMouseSectionIndex := Value;
end;

procedure TcxCustomHeader.UpdateDraggedSection;
begin
  if UnderMouseSectionIndex <> -1 then
  begin
    SetScreenCursor(crDrag);
    DoSectionDrag(SelectedSection, UnderMouseSection);
  end
  else
    RestoreScreenCursor;
end;

procedure TcxCustomHeader.UpdatePressedSection;
begin
  if UnderMouseSectionIndex <> FSelectedSectionIndex then
    SetSectionState(FSelectedSectionIndex, cxbsNormal)
  else
    SetSectionState(FSelectedSectionIndex, cxbsPressed);
end;

procedure TcxCustomHeader.UpdateSizedSection;
var
  ASectionRect: TRect;
  APoint: TPoint;
  ASizedSectionIndex, AUnderMouseSectionIndex: Integer;
begin
  GetCursorPos(APoint);
  APoint := ScreenToClient(APoint);
  ASizedSectionIndex := -1;
  AUnderMouseSectionIndex := GetSectionIndexAtPos(APoint.X, APoint.Y, True);
  if AUnderMouseSectionIndex >= 0 then
  begin
    ASectionRect := GetSectionRect(AUnderMouseSectionIndex);
    if (AUnderMouseSectionIndex < FSections.Count) and
      (UseRightToLeftAlignment and (APoint.X <= ASectionRect.Left + 7) and (APoint.X >= ASectionRect.Left) or
        not UseRightToLeftAlignment and (APoint.X >= ASectionRect.Right - 7) and (APoint.X <= ASectionRect.Right)) then
      ASizedSectionIndex := AUnderMouseSectionIndex
    else
      if (AUnderMouseSectionIndex > 0) and
        (UseRightToLeftAlignment and (APoint.X <= ASectionRect.Right) and (APoint.X >= ASectionRect.Right - 7) or
         not UseRightToLeftAlignment and (APoint.X >= ASectionRect.Left) and (APoint.X <= ASectionRect.Left + 7)) then
        ASizedSectionIndex := AUnderMouseSectionIndex - 1;
    if (ASizedSectionIndex > -1) and not Sections[ASizedSectionIndex].AllowResize then
      ASizedSectionIndex := -1;
  end;
  SizedSectionIndex := ASizedSectionIndex;
end;

procedure TcxCustomHeader.UpdateUnderMouseSection;
var
  APoint: TPoint;
begin
  GetCursorPos(APoint);
  APoint := ScreenToClient(APoint);
  UnderMouseSectionIndex := GetSectionIndexAtPos(APoint.X, APoint.Y);
end;

procedure TcxCustomHeader.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  BeginMouseTracking(Self, ClientRect, Self);
end;

procedure TcxCustomHeader.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  EndMouseTracking(Self);
  case FState of
    hsNone:
      begin
        UnderMouseSectionIndex := -1;
        SizedSectionIndex := -1;
        HotSectionIndex := -1;
      end;
  end;
end;

procedure TcxCustomHeader.SetAllowMultiSort(Value: Boolean);
begin
  if FAllowMultiSort <> Value then
  begin
    FAllowMultiSort := Value;
    if not FAllowMultiSort and (Sections.FirstSortedSection <> nil) then
      DisableSort(Sections.FirstSortedSection);
  end;
end;

procedure TcxCustomHeader.SetAllowSort(Value: Boolean);
begin
  if FAllowSort <> Value then
  begin
    FAllowSort := Value;
    if not FAllowSort then DisableSort;
  end;
end;

procedure TcxCustomHeader.DisableSort(AExceptSection: TcxHeaderSection);
var
  I: Integer;
begin
  Sections.BeginUpdate;
  FSortingChanging := True;
  try
    for I := 0 to Sections.Count - 1 do
      if Sections[I] <> AExceptSection then
        Sections[I].SortOrder := soNone;
  finally
    FSortingChanging := False;
    Sections.EndUpdate;
  end;
end;

procedure TcxCustomHeader.FitToClientWidth;
var
  ASectionWidths: TcxHeaderSectionWidths;
  ASectionWidthsChanged: Boolean;
  I: Integer;
begin
  if not HandleAllocated or (csReading in ComponentState) then
    Exit;
  CalcSectionWidths(Width, ASectionWidths);
  ASectionWidthsChanged := False;
  for I := 0 to Sections.Count - 1 do
    if Sections[I].Width <> ASectionWidths[I] then
    begin
      ASectionWidthsChanged := True;
      Break;
    end;
  if ASectionWidthsChanged then
  begin
    FSectionsFitCalculating := True;
    try
      Sections.BeginUpdate;
      try
        for I := 0 to Sections.Count - 1 do
          Sections[I].Width := ASectionWidths[I];
      finally
        Sections.EndUpdate;
      end;
    finally
      FSectionsFitCalculating := False;
    end;
  end;
end;

function TcxCustomHeader.GetBackgroundColor: TColor;
begin
  if Color = clBtnFace then
    Result := LookAndFeelPainter.DefaultHeaderColor
  else
    Result := clDefault;

  if Result = clDefault then
    Result := Color;
end;

function TcxCustomHeader.GetSectionContentBounds(ASectionBounds: TRect; AState: TcxButtonState): TRect;
begin
  Result := LookAndFeelPainter.HeaderControlSectionContentBounds(ASectionBounds, AState);
end;

function TcxCustomHeader.GetSplitLineOffset: Integer;
begin
  if UseRightToLeftAlignment then
    Result := 1
  else
    Result := Ord(LookAndFeelPainter.LookAndFeelStyle in [lfsStandard, lfsFlat, lfsNative, lfsSkin]);
end;

function TcxCustomHeader.GetTextColor: TColor;
begin
  if Font.Color = clWindowText then
    Result := LookAndFeelPainter.DefaultHeaderTextColor
  else
    Result := clDefault;

  if Result = clDefault then
    Result := Font.Color;
end;

function TcxCustomHeader.IsInnerControl: Boolean;
begin
  Result := False;
end;

{ TcxHeaderSection }

constructor TcxHeaderSection.Create(Collection: TCollection);
begin
  FWidth := 50;
  FMaxWidth := 1000;
  FMinWidth := 30;
  FAllowClick := False;
  FImageIndex := -1;
  FParentBiDiMode := True;
  inherited Create(Collection);
  ParentBiDiModeChanged;
  FDataIndex := Index;
  FAllowResize := True;
end;

procedure TcxHeaderSection.Assign(Source: TPersistent);
var
  ASection: TcxHeaderSection;
begin
  if Source is TcxHeaderSection then
  begin
    ASection := TcxHeaderSection(Source);
    Alignment := ASection.Alignment;
    AllowClick := ASection.AllowClick;
    AllowResize := ASection.AllowResize;
    AutoSize := ASection.AutoSize;
    BiDiMode := ASection.BiDiMode;
    DataIndex := ASection.DataIndex;
    ImageIndex := ASection.ImageIndex;
    MinWidth := ASection.MinWidth;
    MaxWidth := ASection.MaxWidth;
    ParentBiDiMode := ASection.ParentBiDiMode;
    SortOrder := ASection.SortOrder;
    Text := ASection.Text;
    Width := ASection.Width;
  end
  else
    inherited Assign(Source);
end;

procedure TcxHeaderSection.SetBiDiMode(Value: TBiDiMode);
begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Changed(False);
  end;
end;

procedure TcxHeaderSection.SetDataIndex(Value: Integer);
begin
  if Value < -1 then
    Value := -1;
  if Value <> FDataIndex then
  begin
    FDataIndex := Value;
    Changed(False);
  end;
end;

function TcxHeaderSection.IsBiDiModeStored: Boolean;
begin
  Result := not FParentBiDiMode;
end;

function TcxHeaderSection.IsDataIndexStored: Boolean;
begin
  Result := FDataIndex <> Index;
end;

function TcxHeaderSection.GetHeaderControl: TcxCustomHeader;
begin
  Result := TcxHeaderSections(Collection).FHeaderControl;
end;

procedure TcxHeaderSection.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    ParentBiDiModeChanged;
  end;
end;

procedure TcxHeaderSection.SetState(Value: TcxButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;
    Changed(False);
  end;
end;

procedure TcxHeaderSection.ParentBiDiModeChanged;
begin
  if FParentBiDiMode then
  begin
    if GetOwner <> nil then
    begin
      BiDiMode := TcxHeaderSections(GetOwner).FHeaderControl.BiDiMode;
      FParentBiDiMode := True;
    end;
  end;
end;

function TcxHeaderSection.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

function TcxHeaderSection.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

procedure TcxHeaderSection.ChangeScale(M, D: Integer);
begin
  FMinWidth := MulDiv(FMinWidth, M, D);
  FMaxWidth := MulDiv(FMaxWidth, M, D);
  FWidth := MulDiv(FWidth, M, D);
end;

function TcxHeaderSection.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TcxHeaderSection.GetLeft: Integer;
var
  I: Integer;
begin
  if UseRightToLeftAlignment then
    Result := Right - Width
  else
  begin
    Result := 0;
    for I := 0 to Index - 1 do
      Inc(Result, TcxHeaderSections(Collection)[I].Width);
  end;
end;

function TcxHeaderSection.GetRight: Integer;
var
  I: Integer;
begin
  if UseRightToLeftAlignment then
  begin
    Result := HeaderControl.Width;
    for I := 0 to Index - 1 do
      Dec(Result, TcxHeaderSections(Collection)[I].Width);
  end
  else
    Result := Left + Width;
end;

procedure TcxHeaderSection.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TcxHeaderSection.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    if TcxHeaderSections(Collection).FHeaderControl <> nil then
      TcxHeaderSections(Collection).FHeaderControl.AdjustSize;
    Changed(True);
  end;
end;

procedure TcxHeaderSection.SetMaxWidth(Value: TcxNaturalNumber);
begin
  Value := Max(Min(Value, High(TcxNaturalNumber)), Low(TcxNaturalNumber));
  if Value < FMinWidth then
    Value := FMinWidth;
  if Value > 10000 then
    Value := 10000;
  FMaxWidth := Value;
  SetWidth(FWidth);
end;

procedure TcxHeaderSection.SetMinWidth(Value: TcxNaturalNumber);
begin
  Value := Max(Min(Value, High(TcxNaturalNumber)), Low(TcxNaturalNumber));
  if Value > FMaxWidth then
    Value := FMaxWidth;
  FMinWidth := Value;
  SetWidth(FWidth);
end;

procedure TcxHeaderSection.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

procedure TcxHeaderSection.SetSortOrder(Value: TcxHeaderSortOrder);
begin
  if HeaderControl.FSortingChanging then
  begin
    FSortOrder := Value;
    Changed(False);
  end
  else
    HeaderControl.ChangeSectionSortOrder(Self, Value, False);
end;

procedure TcxHeaderSection.SetWidth(Value: Integer);
begin
  if Value < FMinWidth then
    Value := FMinWidth;
  if Value > FMaxWidth then
    Value := FMaxWidth;
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(True);

    if Collection <> nil then
    begin
      TcxHeaderSections(Collection).FHeaderControl.DoSectionResizeEvent(Self);
      TcxHeaderSections(Collection).FHeaderControl.DoSectionEndResizeEvent(Self);
    end;
  end;
end;

procedure TcxHeaderSection.SetImageIndex(const Value: TcxImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

{ TcxHeaderSections }

constructor TcxHeaderSections.Create(HeaderControl: TcxCustomHeader);
begin
  inherited Create(TcxHeaderSection);
  FHeaderControl := HeaderControl;
end;

procedure TcxHeaderSections.Assign(Source: TPersistent);
var
  FHeaderSection: TcxHeaderSection;
  I: Integer;
begin
  if (Source is TcxHeaderSections) then
  begin
    Clear;
    for I := 0 to (Source as TcxHeaderSections).Count - 1 do
    begin
      FHeaderSection := Add;
      FHeaderSection.Assign((Source as TcxHeaderSections).Items[I]);
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxHeaderSections.Add: TcxHeaderSection;
begin
  Result := TcxHeaderSection.Create(Self);
end;

function TcxHeaderSections.GetItem(Index: Integer): TcxHeaderSection;
begin
  Result := TcxHeaderSection(inherited GetItem(Index));
end;

procedure TcxHeaderSections.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

function TcxHeaderSections.GetOwner: TPersistent;
begin
  Result := FHeaderControl;
end;

procedure TcxHeaderSections.SetItem(Index: Integer; Value: TcxHeaderSection);
begin
  inherited SetItem(Index, Value);
end;

function TcxHeaderSections.GetFirstSortedSection: TcxHeaderSection;
begin
  Result := GetSortedSection(0);
end;

function TcxHeaderSections.GetSortedSection(AIndexFrom: Integer): TcxHeaderSection;
var
  I: Integer;
begin
  Result := nil;
  for I := AIndexFrom to Count - 1 do
    if Items[I].SortOrder <> soNone then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TcxHeaderSections.Update(Item: TCollectionItem);
begin
  if FHeaderControl <> nil then
  begin
    if Item <> nil then
    begin
      FHeaderControl.UpdateSection(Item.Index);
      FHeaderControl.DoSectionChange;
    end
    else
    begin
      if not FHeaderControl.FSectionsFitCalculating then
        FHeaderControl.FitToClientWidth;
      FHeaderControl.UpdateSections;
      FHeaderControl.DoSectionsChange;
    end;
  end;
end;

function TcxHeaderSections.Insert(Index: Integer): TcxHeaderSection;
begin
  BeginUpdate;
  try
    if Index < 0 then Index := 0;
    if Index > Count then Index := Count - 1;

    Result := Add;
    Result.Index := Index;
  finally
    EndUpdate;
  end;
end;

end.
