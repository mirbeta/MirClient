{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxFcEditAdv;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Menus, Buttons,
  ImgList, ShellAPI, dxflchrt, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxButtons, cxTextEdit,
  cxImageList, cxClasses, dxColorDialog, cxGeometry, dxBar, dxFcEdit;

type

  TFChartAdvEditor = class(TdxFlowChartCustomCustomizeForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    btnCancel: TcxButton;
    btnCreateConnect: TcxButton;
    btnCreateObject: TcxButton;
    btnOK: TcxButton;
    bvlSpace1: TBevel;
    bvlSpace2: TBevel;
    bvlSpace3: TBevel;
    bvlSpace4: TBevel;
    Chart: TdxFlowChart;
    ColorDialog: TdxColorDialog;
    FontDialog: TFontDialog;
    ilLargeImages: TcxImageList;
    ilSmallImages: TcxImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pAqua: TPanel;
    pBk: TPanel;
    pBkColor: TPanel;
    pBlack: TPanel;
    pBlue: TPanel;
    pColor: TPanel;
    pConnect: TPanel;
    pFuchsia: TPanel;
    pGray: TPanel;
    pGreen: TPanel;
    pLime: TPanel;
    plPalette: TPanel;
    pMarron: TPanel;
    pNavy: TPanel;
    pObject: TPanel;
    pOlive: TPanel;
    pPurple: TPanel;
    pRed: TPanel;
    pTeal: TPanel;
    pWhite: TPanel;
    pYellow: TPanel;
    SaveDialog: TSaveDialog;
    sbConnectFont: TcxButton;
    sbDArrowSize: TcxButton;
    sbDestArrow: TcxButton;
    sbDPoint: TcxButton;
    sbFit: TcxButton;
    sbImagePosition: TcxButton;
    sbLine: TcxButton;
    sbObjectFont: TcxButton;
    sbSArrowSize: TcxButton;
    sbShape: TcxButton;
    sbSourceArrow: TcxButton;
    sbSPoint: TcxButton;
    sbStyle: TcxButton;
    sbTextPosition: TcxButton;
    sbZoom: TcxButton;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    miOpen: TdxBarButton;
    miSaveAs: TdxBarButton;
    miFile: TdxBarSubItem;
    miUndo: TdxBarButton;
    miCut: TdxBarButton;
    miCopy: TdxBarButton;
    miPaste: TdxBarButton;
    miDelete: TdxBarButton;
    miSelectAll: TdxBarButton;
    miClearSelection: TdxBarButton;
    miBringToFront: TdxBarButton;
    miSendToBack: TdxBarButton;
    miEdit: TdxBarSubItem;
    miAntialiasing: TdxBarButton;
    miZoomIn: TdxBarButton;
    miZoomOut: TdxBarButton;
    miFit: TdxBarButton;
    miActualSize: TdxBarButton;
    View1: TdxBarSubItem;
    miNewUnion: TdxBarButton;
    miAddToUnion: TdxBarButton;
    miRemoveFromUnion: TdxBarButton;
    miClearUnion: TdxBarButton;
    miClearAllUnions: TdxBarButton;
    miUnions: TdxBarSubItem;
    miDynamicMoving: TdxBarButton;
    miDynamicSizing: TdxBarButton;
    miOptions: TdxBarSubItem;
    Contents1: TdxBarButton;
    miHelp: TdxBarSubItem;
    bpmShape: TdxBarPopupMenu;
    iNone: TdxBarButton;
    iRectangle: TdxBarButton;
    iEllipse: TdxBarButton;
    iRoundRect: TdxBarButton;
    iDiamond: TdxBarButton;
    iNorthTriangle: TdxBarButton;
    itSouthTriangle: TdxBarButton;
    itEastTriangle: TdxBarButton;
    itWestTriangle: TdxBarButton;
    itHexagon: TdxBarButton;
    bpmChart: TdxBarPopupMenu;
    iEdit: TdxBarButton;
    iCut: TdxBarButton;
    iCopy: TdxBarButton;
    iPaste: TdxBarButton;
    iDelete: TdxBarButton;
    iRemovePoint: TdxBarButton;
    iSelectAll: TdxBarButton;
    iClearSelection: TdxBarButton;
    iBringToFront: TdxBarButton;
    iSendToBack: TdxBarButton;
    iNewUnion: TdxBarButton;
    iAddToUnion: TdxBarButton;
    iRemoveFromUnion: TdxBarButton;
    iClearUnion: TdxBarButton;
    iClearAllUnions: TdxBarButton;
    bpmDArrowSize: TdxBarPopupMenu;
    iD10: TdxBarButton;
    iD20: TdxBarButton;
    iD30: TdxBarButton;
    iD40: TdxBarButton;
    iD50: TdxBarButton;
    bpmDestArrow: TdxBarPopupMenu;
    iDNone: TdxBarButton;
    iDArrow: TdxBarButton;
    iDOvalArrow: TdxBarButton;
    iDRectArrow: TdxBarButton;
    bpmDPoint: TdxBarPopupMenu;
    iDP1: TdxBarButton;
    iDP2: TdxBarButton;
    iDP3: TdxBarButton;
    iDP4: TdxBarButton;
    iDP5: TdxBarButton;
    iDP6: TdxBarButton;
    iDP7: TdxBarButton;
    iDP8: TdxBarButton;
    iDP9: TdxBarButton;
    iDP10: TdxBarButton;
    iDP11: TdxBarButton;
    iDP12: TdxBarButton;
    iDP13: TdxBarButton;
    iDP14: TdxBarButton;
    iDP15: TdxBarButton;
    iDP16: TdxBarButton;
    bpmImagePosition: TdxBarPopupMenu;
    iImageTopLeft: TdxBarButton;
    iImageTop: TdxBarButton;
    iImageTopRight: TdxBarButton;
    iImageLeft: TdxBarButton;
    iImageCenter: TdxBarButton;
    iImageRight: TdxBarButton;
    iImageBottomLeft: TdxBarButton;
    iImageBottom: TdxBarButton;
    iImageBottomRight: TdxBarButton;
    bpmLine: TdxBarPopupMenu;
    i1p: TdxBarButton;
    i2p: TdxBarButton;
    i3p: TdxBarButton;
    i4p: TdxBarButton;
    i5p: TdxBarButton;
    i6p: TdxBarButton;
    i7p: TdxBarButton;
    i8p: TdxBarButton;
    i9p: TdxBarButton;
    i10p: TdxBarButton;
    bpmSArrowSize: TdxBarPopupMenu;
    iS10: TdxBarButton;
    iS20: TdxBarButton;
    iS30: TdxBarButton;
    iS40: TdxBarButton;
    iS50: TdxBarButton;
    bpmSourceArrow: TdxBarPopupMenu;
    iSNone: TdxBarButton;
    iSArrow: TdxBarButton;
    iSOvalArrow: TdxBarButton;
    iSRectArrow: TdxBarButton;
    bpmSPoint: TdxBarPopupMenu;
    iSP1: TdxBarButton;
    iSP2: TdxBarButton;
    iSP3: TdxBarButton;
    iSP4: TdxBarButton;
    iSP5: TdxBarButton;
    iSP6: TdxBarButton;
    iSP7: TdxBarButton;
    iSP8: TdxBarButton;
    iSP9: TdxBarButton;
    iSP10: TdxBarButton;
    iSP11: TdxBarButton;
    iSP12: TdxBarButton;
    iSP13: TdxBarButton;
    iSP14: TdxBarButton;
    iSP15: TdxBarButton;
    iSP16: TdxBarButton;
    bpmStyle: TdxBarPopupMenu;
    iStraight: TdxBarButton;
    iCurved: TdxBarButton;
    iRectHorizontal: TdxBarButton;
    iRectVertical: TdxBarButton;
    bpmTextPosition: TdxBarPopupMenu;
    iTextTopLeft: TdxBarButton;
    iTextTop: TdxBarButton;
    iTextTopRight: TdxBarButton;
    iTextLeft: TdxBarButton;
    iTextCenter: TdxBarButton;
    iTextRight: TdxBarButton;
    iTextBottomLeft: TdxBarButton;
    iTextBottom: TdxBarButton;
    iTextBottomRight: TdxBarButton;

    procedure btnCreateConnectClick(Sender: TObject);
    procedure btnCreateObjectClick(Sender: TObject);
    procedure ChartChange(Sender: TdxCustomFlowChart; Item: TdxFcItem);
    procedure ChartDblClick(Sender: TObject);
    procedure ChartKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ChartKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChartPopupMenuPopup(Sender: TObject);
    procedure ChartSelected(Sender: TdxCustomFlowChart; Item: TdxFcItem);
    procedure ChartSelection(Sender: TdxCustomFlowChart; Item: TdxFcItem; var Allow: Boolean);
    procedure Contents1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure iAddToUnionClick(Sender: TObject);
    procedure iBringToFrontClick(Sender: TObject);
    procedure iClearAllUnionsClick(Sender: TObject);
    procedure iClearSelectionClick(Sender: TObject);
    procedure iClearUnionClick(Sender: TObject);
    procedure iCopyClick(Sender: TObject);
    procedure iCutClick(Sender: TObject);
    procedure iDeleteClick(Sender: TObject);
    procedure iEditClick(Sender: TObject);
    procedure iNewUnionClick(Sender: TObject);
    procedure iPasteClick(Sender: TObject);
    procedure iRectangleClick(Sender: TObject);
    procedure iRemoveFromUnionClick(Sender: TObject);
    procedure iRemovePointClick(Sender: TObject);
    procedure iSelectAllClick(Sender: TObject);
    procedure iSendToBackClick(Sender: TObject);
    procedure miActualSizeClick(Sender: TObject);
    procedure miAntialiasingClick(Sender: TObject);
    procedure miDynamicMovingClick(Sender: TObject);
    procedure miDynamicSizingClick(Sender: TObject);
    procedure miFitClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miZoomInClick(Sender: TObject);
    procedure miZoomOutClick(Sender: TObject);
    procedure pBlackMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pColorClick(Sender: TObject);
    procedure pColorDblClick(Sender: TObject);
    procedure sbConnectFontClick(Sender: TObject);
    procedure sbFitClick(Sender: TObject);
    procedure sbObjectFontClick(Sender: TObject);
    procedure sbShapeClick(Sender: TObject);
    procedure sbZoomClick(Sender: TObject);
  private
    Buf: TList;
    BufChart: TdxFlowChart;
    DownPoint: TPoint;
    FChange: Boolean;
    FConnectionFont: TFont;
    FNewObject: Boolean;
    FObjectFont: TFont;
    FPE: Boolean;
    FStore: Boolean;
    FUndo: TUndoManager;
    LastObj: TdxFcObject;
    OldHintHidePause : Integer;
    OldPoint: TPoint;

    function GetImageIndexByMenuItem(Item: TdxBarItem) : Integer;
    procedure ChangeConnections(Mode : Integer);
    procedure ChangeObjects(Mode : Integer);
    procedure ClearBuf;
    procedure ConAssign(Source, Dest : TdxFcConnection);
    procedure CopyToBuf;
    procedure DrawDrag(P1, P2 : TPoint; Mode : Integer);
    procedure InitializeMenuImageIndexes;
    procedure ObjAssign(Source, Dest : TdxFcObject);
    procedure PasteFromBuf;
    procedure SelectLastConnect;
    procedure SelectLastObject;
    procedure SetGlyph(SB : TcxButton; PM : TdxBarPopupMenu);

    procedure LoadLocalizations;
    procedure LocalizeArrowStyleMenu(AMenu: TdxBarPopupMenu);
    procedure LocalizeIndexesMenu(AMenu: TdxBarPopupMenu; const AFormatString: string);
    procedure LocalizeLayoutMenu(AMenu: TdxBarPopupMenu);
    procedure LocalizeShapeMenu;
    procedure LocalizeStyleMenu;

    procedure Changing;
    procedure Changed;
  public
    function Execute(AChart: TdxFlowChart): Boolean; override;

    function CanKeyEnter(Edit: TcxTextEdit; Key : Char; MinValue, MaxValue : Integer) : Boolean;
    function ChartHasUnions(AChart : TdxFlowChart) : Boolean;
    function GetMainItemInUnion(AChart : TdxFlowChart; Obj : TdxFcObject) : TdxFcObject;
    function GetUnionByNumber(AChart : TdxFlowChart; Number : Integer) : TdxFcObject;
    function IsMainItemInUnion(Obj : TdxFcObject) : Boolean;
  end;

implementation

uses
  dxEditObj, dxEditCon, dxSelUnion, dxCore, dxFcStrs, Math, dxCoreGraphics, dxDPIAwareUtils;

{$R *.DFM}

type
  TdxFlowChartAccess = class(TdxFlowChart);

{ TFChartEditor }

procedure TFChartAdvEditor.ChartMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  hTest: TdxFcHitTest;
  R: TRect;
begin
  if Button = mbLeft then
  begin
    DownPoint := Point(X, Y);
    OldPoint := DownPoint;
  end;

  if sbZoom.Down then
  begin
    if ssAlt in Shift then
    begin
      if Chart.Zoom > 20 then
        Chart.Zoom := Chart.Zoom - 10
    end
    else
    begin
      if Chart.Zoom < 490 then
        Chart.Zoom := Chart.Zoom + 10;
    end;
    miActualSize.Enabled := (Chart.Zoom <> 100) and (not sbFit.Down);
    exit;
  end;

  hTest := Chart.GetHitTestAt(X,Y);
  if (hTest = [htNowhere]) and (Button = mbLeft) and (not (ssShift in Shift)) then
    Chart.ClearSelection;
  if (btnCreateObject.Down or btnCreateConnect.Down) and (Button = mbLeft) then
  begin
    FNewObject := True;
    R := Chart.ClientRect;
    R.TopLeft := Chart.ClientToScreen(R.TopLeft);
    R.BottomRight := Chart.ClientToScreen(R.BottomRight);
    ClipCursor(@R);
    Chart.ClearSelection;
  end;
  if FPE then
    FPE := False;
end;

procedure TFChartAdvEditor.ChartMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ALeft, ATop, SPoint, DPoint: integer;
  P1, P2 : TPoint;
  SObj, DObj, Obj: TdxFcObject;
  Con : TdxFcConnection;
begin
  if sbZoom.Down then exit;

  if btnCreateObject.Down then
  begin
    btnCreateObject.Down := False;
    Chart.Options := Chart.Options + [fcoMultiSelect];
    DrawDrag(DownPoint, OldPoint, 1);
    if (Button = mbLeft) and (Abs(DownPoint.X - X) > 5) and (Abs(DownPoint.Y - Y) > 5) then
    begin
      FChange := False;
      P1 := Chart.ChartPoint(DownPoint.X, DownPoint.Y);
      P2 := Chart.ChartPoint(X, Y);
      if P1.X < P2.X then
        ALeft := P1.X
      else
        ALeft := P2.X;

      if P1.Y < P2.Y then
        ATop := P1.Y
      else
        ATop := P2.Y;

      FStore := False;
      Chart.CreateObject(ALeft , ATop, Abs(P1.X - P2.X), Abs(P1.Y - P2.Y), TdxFcShapeType(sbShape.Tag));
      SelectLastObject;
      ChangeObjects(0);
      FChange := True;
      Obj := Chart.Objects[Chart.ObjectCount-1];
      if Obj.Text = '' then
        Obj.Text := cxGetResourceString(@sdxFlowChartEditorObject) + ' ' + IntToStr(Chart.ObjectCount);
      FStore := True;
      ChartChange(Chart, nil);
    end;
  end;
  if btnCreateConnect.Down then
  begin
    btnCreateConnect.Down := False;
    Chart.Options := Chart.Options + [fcoMultiSelect];
    DrawDrag(DownPoint, OldPoint, 2);
    if (Button = mbLeft) and ((Abs(DownPoint.X - X) > 5) or (Abs(DownPoint.Y - Y) > 5)) then
    begin
      FChange := False;
      FStore := False;
      SObj := Chart.GetObjectAt(DownPoint.X, DownPoint.Y);
      DObj := Chart.GetObjectAt(X, Y);
      if SObj <> nil then SPoint := SObj.GetLinkedPoint(DownPoint.X, DownPoint.Y) else SPoint := sbSPoint.Tag;
      if DObj <> nil then DPoint := DObj.GetLinkedPoint(X, Y) else DPoint := sbDPoint.Tag;
      Chart.CreateConnection(SObj, DObj , SPoint, DPoint);
      Con := Chart.Connections[Chart.ConnectionCount-1];
      if Chart.GetObjectAt(DownPoint.X, DownPoint.Y) = nil then
        Con.AddPoint(Chart.ChartPoint(DownPoint.X, DownPoint.Y));
      if Chart.GetObjectAt(X, Y) = nil then
        Con.AddPoint(Chart.ChartPoint(X, Y));
      SelectLastConnect;
      sbSPoint.Tag := SPoint;
      sbDPoint.Tag := DPoint;
      ChangeConnections(0);
      FChange := True;
      SelectLastConnect;
      FStore := True;
      ChartChange(Chart, nil);
    end;
  end;
  FNewObject := False;
  ClipCursor(nil);

  if Button = mbRight then
  begin
    if Chart.SelCount = 0 then
      if Chart.GetConnectionAt(X, Y) <> nil then
        Chart.GetConnectionAt(X, Y).Selected := True;

    if Chart.SelCount = 0 then
      if Chart.GetObjectAt(X, Y) <> nil then
        Chart.GetObjectAt(X, Y).Selected := True;
  end;

  if Button = mbRight then
  begin
    P1 := Chart.ClientToScreen(Point(X, Y));
    DownPoint := Point(X, Y);
    bpmChart.Popup(P1.X, P1.Y);
  end;
end;

procedure TFChartAdvEditor.ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  function GetUnionString(ACount: Integer): string;
  begin
    if ACount > 1 then
      Result := cxGetResourceString(@sdxFlowChartUnions)
    else
      Result := cxGetResourceString(@sdxFlowChartUnion);
  end;

var
  UpPoint: TPoint;
  Obj : TdxFcObject;
  AText, CountText : String;
  FirstObj : TdxFcObject;
  Count : integer;
begin
  if sbZoom.Down then exit;

  UpPoint := Point(X, Y);
  if btnCreateObject.Down and FNewObject then
  begin
    DrawDrag(DownPoint, OldPoint, 1);
    DrawDrag(DownPoint, UpPoint, 1);
    OldPoint := UpPoint;
  end;

  if btnCreateConnect.Down and FNewObject and
    ((Abs(UpPoint.Y - DownPoint.Y) > 5) or (Abs(UpPoint.X - DownPoint.X) > 5)) then
  begin
    DrawDrag(DownPoint, OldPoint, 2);
    DrawDrag(DownPoint, UpPoint, 2);
    OldPoint := UpPoint;
  end;

  if not (btnCreateConnect.Down or btnCreateObject.Down) then
  begin
    Obj := Chart.GetObjectAt(X, Y);
    AText := '';
    if Obj <> nil then
    begin
      if IsMainItemInUnion(Obj) then
        AText := Format(cxGetResourceString(@sdxFlowChartEditorMainItemOfUnion),
          [GetNumberByUnion(Chart, Obj)]);

      if IsChildItemInUnion(Chart, Obj) then
      begin
        Count := 0;
        CountText := '';
        FirstObj := nil;
        repeat
          FirstObj := FindUnions(Chart, FirstObj, Obj);
          if FirstObj <> nil then
          begin
            if Count = 0 then
              CountText := CountText + IntToStr(GetNumberByUnion(Chart, FirstObj))
            else
              CountText := CountText + ', ' + IntToStr(GetNumberByUnion(Chart, FirstObj));
            Inc(Count);
          end;
        until FirstObj = nil;

        if Count > 0 then
        begin
          if AText <> '' then
            AText := AText + ', ';
          AText := AText + cxGetResourceString(@sdxFlowChartEditorChildItem);
          AText := Format(AText, [GetUnionString(Count)]);
          AText := AText + CountText;
        end;
      end;
    end;
    if AText = '' then
      Obj := nil;
    if (Obj <> nil) and (Obj <> LastObj) then
    begin
      Application.HintHidePause := 5000;
      Chart.Hint := AText;
      Chart.ShowHint := True;
      LastObj := Obj;
    end
    else
      if Obj <> LastObj then
      begin
        Chart.ShowHint := False;
        LastObj := nil;
        Application.HintHidePause := OldHintHidePause;
      end;
  end;
end;

procedure TFChartAdvEditor.sbShapeClick(Sender: TObject);
var
  P : TPoint;
  APanel : TPanel;
  APopupMenu : TdxBarPopupMenu;
begin
  APanel := nil;
  APopupMenu := nil;
  if Sender = sbShape then
  begin
    APanel := pObject;
    APopupMenu := bpmShape;
  end;
  if Sender = sbLine then
  begin
    APanel := pObject;
    APopupMenu := bpmLine;
  end;
  if Sender = sbTextPosition then
  begin
    APanel := pObject;
    APopupMenu := bpmTextPosition;
  end;
  if Sender = sbImagePosition then
  begin
    APanel := pObject;
    APopupMenu := bpmImagePosition;
  end;
  if Sender = sbStyle then
  begin
    APanel := pConnect;
    APopupMenu := bpmStyle;
  end;
  if Sender = sbSourceArrow then
  begin
    APanel := pConnect;
    APopupMenu := bpmSourceArrow;
  end;
  if Sender = sbDestArrow then
  begin
    APanel := pConnect;
    APopupMenu := bpmDestArrow;
  end;
  if Sender = sbSArrowSize then
  begin
    APanel := pConnect;
    APopupMenu := bpmSArrowSize;
  end;
  if Sender = sbDArrowSize then
  begin
    APanel := pConnect;
    APopupMenu := bpmDArrowSize;
  end;
  if Sender = sbSPoint then
  begin
    APanel := pConnect;
    APopupMenu := bpmSPoint;
  end;
  if Sender = sbDPoint then
  begin
    APanel := pConnect;
    APopupMenu := bpmDPoint;
  end;

  if APanel = nil then Exit;

  with TcxButton(Sender) do
    P := APanel.ClientToScreen(Point(Left, Top + Height + 2));
  APopupMenu.Popup(P.X, P.Y);
end;

procedure TFChartAdvEditor.FormCreate(Sender: TObject);

  function CreateFont: TFont;
  begin
    Result := TFont.Create;
    Result.Assign(Chart.Font);
  end;

  procedure ExcludeParentBackground(APanel: TPanel);
  begin
    APanel.ControlStyle := APanel.ControlStyle - [csParentBackground];
  end;

var
  I: Integer;
begin
  FObjectFont := CreateFont;
  FConnectionFont := CreateFont;

  FChange := True;
  Buf := TList.Create;
  BufChart := TdxFlowChart.Create(Self);
  FUndo := TUndoManager.Create(Chart, 30);
  FStore := True;
  LastObj := nil;
  OldHintHidePause := Application.HintHidePause;
  InitializeMenuImageIndexes;
  LoadLocalizations;

  ExcludeParentBackground(pColor);
  ExcludeParentBackground(pBkColor);
  for I := 0 to plPalette.ControlCount - 1 do
  begin
    if plPalette.Controls[I] is TPanel then
      ExcludeParentBackground(TPanel(plPalette.Controls[I]));
  end;

  with Constraints do
  begin
    MinHeight := MulDiv(MinHeight, PixelsPerInch, 96);
    MinWidth := MulDiv(MinWidth, PixelsPerInch, 96);
  end;
end;

procedure TFChartAdvEditor.ChangeObjects(Mode: Integer);
var
  AChange: Boolean;
  APrevStore: Boolean;
  I, Position: Integer;
begin
  AChange := False;
  APrevStore := FStore;
  try
    FStore := False;
    for i := 0 to Chart.SelectedObjectCount - 1 do
    begin
      with Chart.SelectedObjects[I] do
      begin
        if Mode in [0, 1] then
          ShapeType := TdxFcShapeType(sbShape.Tag);
        if Mode in [0, 2] then
          ShapeWidth := sbLine.Tag;
        if Mode in [0, 3] then
          ShapeColor := pColor.Color;
        if Mode in [0, 4] then
          BkColor := pBkColor.Color;
        if Mode in [0, 5] then
        begin
          Position := sbTextPosition.Tag;
          HorzTextPos := TdxFcHorzPos(Position mod 3);
          VertTextPos := TdxFcVertPos(Position div 3);
        end;
        if Mode in [0, 6] then
        begin
          Position := sbImagePosition.Tag;
          HorzImagePos := TdxFcHorzPos(Position mod 3);
          VertImagePos := TdxFcVertPos(Position div 3);
        end;
        if Mode in [0, 7] then
          Font.Assign(FObjectFont);
        AChange := True;
      end;
    end;
  finally
    if APrevStore and AChange then
      ChartChange(Chart, nil);
    FStore := APrevStore;
  end;
end;

procedure TFChartAdvEditor.ChangeConnections(Mode : Integer);
var
  APrevStore, AChange: Boolean;
  I: integer;
begin
  AChange := False;
  APrevStore := FStore;
  try
    FStore := False;
    for I := 0 to Chart.SelectedConnectionCount - 1 do
    begin
      with Chart.SelectedConnections[I] do
      begin
        if Mode in [0,1] then Style := TdxFclStyle(sbStyle.Tag - 1);
        if Mode in [0,2] then Color := pColor.Color;
        if Mode in [0,3] then ArrowSource.ArrowType := TdxFcaType(sbSourceArrow.Tag);
        if Mode in [0,4] then ArrowDest.ArrowType := TdxFcaType(sbDestArrow.Tag);
        if Mode in [0,5] then ArrowSource.Width := sbSArrowSize.Tag * 5 + 5;
        if Mode in [0,5] then ArrowSource.Height := sbSArrowSize.Tag * 5 + 5;
        if Mode in [0,6] then ArrowDest.Width := sbDArrowSize.Tag * 5 + 5;
        if Mode in [0,6] then ArrowDest.Height := sbDArrowSize.Tag * 5 + 5;
        if Mode in [0,7] then SetObjectSource(ObjectSource,sbSPoint.Tag);
        if Mode in [0,8] then SetObjectDest(ObjectDest, sbDPoint.Tag);
        if Mode in [0,9] then Font.Assign(FConnectionFont);
        if Mode in [0,10] then ArrowSource.Color := pBkColor.Color;
        if Mode in [0,10] then ArrowDest.Color := pBkColor.Color;
        AChange := True;
      end;
    end;
  finally
    if APrevStore and AChange then
      ChartChange(Chart, nil);
    FStore := APrevStore;
  end;
end;

procedure TFChartAdvEditor.iRectangleClick(Sender: TObject);
var
  APopupMenu: TdxBarPopupMenu;
  ASB: TcxButton;

  function GetPopupMenuByItem(AItem: TdxBarItem): TdxBarPopupMenu;
  var
    AComponent: TComponent;
    I, J: Integer;
  begin
    for I := 0 to ComponentCount - 1 do
    begin
      AComponent := Components[I];
      if AComponent is TdxBarPopupMenu then
        for J := 0 to TdxBarPopupMenu(AComponent).ItemLinks.Count - 1 do
        begin
          if TdxBarPopupMenu(AComponent).ItemLinks[J].Item = AItem then
            Exit(TdxBarPopupMenu(AComponent))
        end;
    end;
    Result := nil;
  end;

begin
  APopupMenu := GetPopupMenuByItem(TdxBarItem(Sender));
  if APopupMenu = nil then exit;

  ASB := nil;
  if APopupMenu = bpmShape then ASB := sbShape;
  if APopupMenu = bpmLine then ASB := sbLine;
  if APopupMenu = bpmStyle then ASB := sbStyle;
  if APopupMenu = bpmSourceArrow then ASB := sbSourceArrow;
  if APopupMenu = bpmDestArrow then ASB := sbDestArrow;
  if APopupMenu = bpmSArrowSize then ASB := sbSArrowSize;
  if APopupMenu = bpmDArrowSize then ASB := sbDArrowSize;
  if APopupMenu = bpmSPoint then ASB := sbSPoint;
  if APopupMenu = bpmDPoint then ASB := sbDPoint;
  if APopupMenu = bpmTextPosition then ASB := sbTextPosition;
  if APopupMenu = bpmImagePosition then ASB := sbImagePosition;

  ASB.Tag := TdxBarItem(Sender).Tag;
  ASB.OptionsImage.ImageIndex := TdxBarItem(Sender).ImageIndex;

  FStore := False;
  try
    if ASB = sbShape then
      ChangeObjects(1);
    if ASB = sbLine then
      ChangeObjects(2);
    if ASB = sbTextPosition then
      ChangeObjects(5);
    if ASB = sbImagePosition then
      ChangeObjects(6);
    if ASB = sbStyle then
      ChangeConnections(1);
    if ASB = sbSourceArrow then
      ChangeConnections(3);
    if ASB = sbDestArrow then
      ChangeConnections(4);
    if ASB = sbSArrowSize then
      ChangeConnections(5);
    if ASB = sbDArrowSize then
      ChangeConnections(6);
    if ASB = sbSPoint then
      ChangeConnections(7);
    if ASB = sbDPoint then
      ChangeConnections(8);
  finally
    FStore := True;
  end;

  ChartChange(Chart, nil);
end;

procedure TFChartAdvEditor.btnCreateConnectClick(Sender: TObject);
begin
  if (Chart.SelectedObjectCount=2) and (Chart.SelectedConnectionCount=0) then
  begin
    with Chart do
    begin
      FStore := False;
      try
        CreateConnection(SelectedObjects[0],SelectedObjects[1], 0, 0);
        Connections[ConnectionCount - 1].Style := TdxFclStyle(sbStyle.Tag - 1);
        SelectLastConnect;
        ChangeConnections(0);
      finally
        FStore := True;
        ChartChange(Chart, nil);
      end;
    end;
    btnCreateConnect.Down := False;
  end
  else
    if TcxButton(Sender).Down then
    begin
      Chart.ClearSelection;
      Chart.Options := Chart.Options - [fcoMultiSelect];
    end;
end;

procedure TFChartAdvEditor.SelectLastObject;
begin
  with Chart do
  begin
    ClearSelection;
    if ObjectCount > 0 then
      Objects[ObjectCount - 1].Selected := True;
  end;
end;

procedure TFChartAdvEditor.SelectLastConnect;
begin
  with Chart do
  begin
    ClearSelection;
    if ConnectionCount > 0 then
      Connections[ConnectionCount - 1].Selected := True;
  end;
end;

procedure TFChartAdvEditor.Changing;
begin
  FStore := False;
  FChange := False;
end;

procedure TFChartAdvEditor.Changed;
begin
  FStore := True;
  FChange := True;
  ChartChange(Chart, nil);
end;

procedure TFChartAdvEditor.pBlackMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Changing;
  try
    if Button = mbLeft then
    begin
      pColor.Color := TPanel(Sender).Color;
      ChangeObjects(3);
      ChangeConnections(2);
    end;
    if Button = mbRight then
    begin
      pBkColor.Color := TPanel(Sender).Color;
      ChangeObjects(4);
      ChangeConnections(10);
    end;
    if (Chart.SelCount = 0) and (Button = mbRight) then
       Chart.Color := pBkColor.Color;
  finally
    Changed;
  end;
end;

procedure TFChartAdvEditor.pColorClick(Sender: TObject);
begin
  Changing;
  try
    if TPanel(Sender) = pColor then
    begin
      ChangeObjects(3);
      ChangeConnections(2);
    end;
    if TPanel(Sender) = pBkColor then
    begin
      ChangeObjects(4);
      ChangeConnections(10);
    end;
  finally
    Changed;
  end;
end;

procedure TFChartAdvEditor.pColorDblClick(Sender: TObject);
begin
  ColorDialog.Color := dxColorToAlphaColor(TPanel(Sender).Color);
  if ColorDialog.Execute then
  begin
    Changing;
    try
      if TPanel(Sender).Tag = 1 then
      begin
        pColor.Color := dxAlphaColorToColor(ColorDialog.Color);
        ChangeObjects(3);
        ChangeConnections(2);
      end;
      if TPanel(Sender).Tag = 2 then
      begin
        pBkColor.Color := dxAlphaColorToColor(ColorDialog.Color);
        ChangeObjects(4);
        ChangeConnections(10);
      end;
      if (Chart.SelCount = 0) and (TPanel(Sender).Tag = 2) then
        Chart.Color := pBkColor.Color;
    finally
      Changed;
    end;
  end;
end;

procedure TFChartAdvEditor.ChartSelected(Sender: TdxCustomFlowChart; Item: TdxFcItem);
var
  En: Boolean;
  I: Integer;
begin
  if not FChange then Exit;
  FChange := False;

  if (Chart.SelCount = 0) and (not btnCreateObject.Down) then
  begin
    if Chart.Color <> clDefault then
      pBkColor.Color := Chart.Color;
  end;

  if Chart.SelectedObjectCount > 0 then
    with Chart.SelectedObjects[0] do
    begin
      if sbShape.Tag <> Integer(ShapeType) then
      begin
        sbShape.Tag := Integer(ShapeType);
        SetGlyph(sbShape, bpmShape);
      end;
      if sbLine.Tag <> ShapeWidth then
      begin
        sbLine.Tag := ShapeWidth;
        SetGlyph(sbLine, bpmLine);
      end;
      pColor.Color := ShapeColor;
      pBkColor.Color := BkColor;
      if sbTextPosition.Tag <> (Integer(VertTextPos) * 3 + Integer(HorzTextPos)) then
      begin
        sbTextPosition.Tag := (Integer(VertTextPos) * 3 + Integer(HorzTextPos));
        SetGlyph(sbTextPosition, bpmTextPosition);
      end;
      if sbImagePosition.Tag <> (Integer(VertImagePos) * 3 + Integer(HorzImagePos)) then
      begin
        sbImagePosition.Tag := (Integer(VertImagePos) * 3 + Integer(HorzImagePos));
        SetGlyph(sbImagePosition, bpmImagePosition);
      end;
      FObjectFont.Assign(Font);
    end;

  if Chart.SelectedConnectionCount > 0 then
    with Chart.SelectedConnections[0] do
    begin
      if sbStyle.Tag <> (Integer(Style) + 1) then
      begin
        sbStyle.Tag:= (Integer(Style) + 1);
        SetGlyph(sbStyle, bpmStyle);
      end;
      pColor.Color := Color;
      pBkColor.Color := ArrowSource.Color;
      if sbSourceArrow.Tag <> Integer(ArrowSource.ArrowType) then
      begin
        sbSourceArrow.Tag := Integer(ArrowSource.ArrowType);
        SetGlyph(sbSourceArrow, bpmSourceArrow);
      end;
      if sbDestArrow.Tag <> Integer(ArrowDest.ArrowType) then
      begin
        sbDestArrow.Tag := Integer(ArrowDest.ArrowType);
        SetGlyph(sbDestArrow, bpmDestArrow);
      end;
      sbSArrowSize.Tag := (ArrowSource.Width - 5) div 5;
      sbDArrowSize.Tag := (ArrowDest.Width - 5) div 5;
      if sbSArrowSize.Tag < 1 then sbSArrowSize.Tag := 1;
      if sbDArrowSize.Tag < 1 then sbDArrowSize.Tag := 1;
      SetGlyph(sbSArrowSize, bpmSArrowSize);
      SetGlyph(sbDArrowSize, bpmDArrowSize);
      sbSPoint.Tag := PointSource;
      SetGlyph(sbSPoint, bpmSPoint);
      sbDPoint.Tag := PointDest;
      SetGlyph(sbDPoint, bpmDPoint);
      FConnectionFont.Assign(Font);
    end;

  if Self.HandleAllocated then
  begin
    En := Chart.SelCount > 0;
    iCut.Enabled := En;
    miCut.Enabled := En;
    iCopy.Enabled := En;
    miCopy.Enabled := En;
    iClearSelection.Enabled := En;
    miClearSelection.Enabled := En;
    iDelete.Enabled := En;
    miDelete.Enabled := En;
    iEdit.Enabled := En;
    iPaste.Enabled := Buf.Count > 0;
    miPaste.Enabled := Buf.Count > 0;

    En := Chart.SelectedObjectCount > 0;
    iBringToFront.Enabled := En;
    miBringToFront.Enabled := En;
    iSendToBack.Enabled := En;
    miSendToBack.Enabled := En;

    En := Chart.SelectedObjectCount > 1 ;
    iNewUnion.Enabled := En;
    miNewUnion.Enabled := En;

    En := ChartHasUnions(Chart) and (Chart.SelectedObjectCount > 0);
    iAddToUnion.Enabled := En;
    miAddToUnion.Enabled := En;

    En := False;
    for i := 0 to Chart.SelectedObjectCount - 1 do
      if IsChildItemInUnion(Chart, Chart.SelectedObjects[i]) then
      begin
        En := True;
        break;
      end;
    iRemoveFromUnion.Enabled := En;
    miRemoveFromUnion.Enabled := En;

    En := False;
    for i := 0 to Chart.SelectedObjectCount - 1 do
      if IsMainItemInUnion(Chart.SelectedObjects[i]) then
        En := True;
    iClearUnion.Enabled := En;
    miClearUnion.Enabled := En;

    En := ChartHasUnions(Chart);
    iClearAllUnions.Enabled := En;
    miClearAllUnions.Enabled := En;

  end;
  FChange := True;
end;

procedure TFChartAdvEditor.ChartSelection(
  Sender: TdxCustomFlowChart; Item: TdxFcItem; var Allow: Boolean);
begin
  Allow := not (sbZoom.Down or btnCreateObject.Down or btnCreateConnect.Down);
end;

procedure TFChartAdvEditor.sbZoomClick(Sender: TObject);
begin
  if TcxButton(Sender).Down then
  begin
    Chart.ClearSelection;
    Chart.Cursor := crFlChartZoomIn;
  end
  else
    Chart.Cursor := crDefault;
end;

procedure TFChartAdvEditor.sbFitClick(Sender: TObject);
begin
  if TcxButton(Sender).Down then
  begin
    sbZoom.Enabled := False;
    sbZoom.Down := False;
    Chart.Zoom := 0;
    miZoomIn.Enabled := False;
    miZoomOut.Enabled := False;
    miActualSize.Enabled := False;
  end
  else
  begin
    sbZoom.Enabled := True;
    Chart.Zoom := 100;
    miZoomIn.Enabled := True;
    miZoomOut.Enabled := True;
    miActualSize.Enabled := True;
  end;
  miActualSize.Enabled := (Chart.Zoom <> 100) and (not sbFit.Down);
  Chart.Cursor := crDefault;
end;

procedure TFChartAdvEditor.sbObjectFontClick(Sender: TObject);
begin
  if Chart.SelectedObjectCount > 0 then
    FObjectFont.Assign(Chart.SelectedObjects[0].Font);
  FontDialog.Font.Assign(FObjectFont);
  if FontDialog.Execute then
  begin
    FObjectFont.Assign(FontDialog.Font);
    ChangeObjects(7);
  end;
end;

procedure TFChartAdvEditor.sbConnectFontClick(Sender: TObject);
begin
  if Chart.SelectedConnectionCount > 0 then
    FConnectionFont.Assign(Chart.SelectedConnections[0].Font);
  FontDialog.Font.Assign(FConnectionFont);
  if FontDialog.Execute then
  begin
    FConnectionFont.Assign(FontDialog.Font);
    ChangeConnections(9);
  end;
end;

procedure TFChartAdvEditor.SetGlyph(SB: TcxButton; PM: TdxBarPopupMenu);
var
  I: Integer;
begin
  for I := 0 to PM.ItemLinks.Count - 1 do
    if SB.Tag = PM.ItemLinks[I].Item.Tag then
    begin
      SB.OptionsImage.ImageIndex := PM.ItemLinks[I].Item.ImageIndex;
      Break;
    end;
end;

procedure TFChartAdvEditor.ChartDblClick(Sender: TObject);
var
  hTest: TdxFcHitTest;
  Res: Boolean;
begin
  hTest := Chart.GetHitTestAt(DownPoint.X, DownPoint.Y);
  if (hTest * [htByObject,htOnObject]) <> [] then
    if Chart.SelectedObjectCount > 0 then
    begin
      FStore := False;
      Res := ObjectEditor(Chart, Chart.SelectedObjects[0]);
      FStore := True;
      FPE := True;
      if Res then
        ChartChange(Chart, nil);
      ChartSelected(Chart, nil);
    end;

  if (hTest * [htOnConnection, htOnConLabel,htOnArrowSrc,htOnArrowDst]) <> [] then
    if Chart.SelectedConnectionCount > 0 then
    begin
      FStore := False;
      Res := ConnectEditor(Chart, Chart.SelectedConnections[0]); // PropertForm
      FStore := True;
      if Res then
        ChartChange(Chart, nil);
      ChartSelected(Chart, nil);
    end;
end;

procedure TFChartAdvEditor.iClearSelectionClick(Sender: TObject);
begin
   Chart.ClearSelection;
end;

procedure TFChartAdvEditor.iSelectAllClick(Sender: TObject);
begin
  Chart.SelectAll;
end;

procedure TFChartAdvEditor.iBringToFrontClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Chart.SelectedObjectCount - 1 do
    Chart.SelectedObjects[I].BringToFront;
end;

procedure TFChartAdvEditor.iSendToBackClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Chart.SelectedObjectCount - 1 do
    Chart.SelectedObjects[I].SendToBack;
end;

procedure TFChartAdvEditor.miOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Chart.LoadFromFile(OpenDialog.FileName);
    ChartChange(Chart, nil);
  end;
end;

procedure TFChartAdvEditor.miSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Chart.SaveToFile(SaveDialog.FileName);
end;

procedure TFChartAdvEditor.iDeleteClick(Sender: TObject);
begin
  Chart.DeleteSelection;
end;

procedure TFChartAdvEditor.FormDestroy(Sender: TObject);
begin
  ClearBuf;
  FreeAndNil(Buf);
  FreeAndNil(BufChart);
  FreeAndNil(FUndo);
  FreeAndNil(FConnectionFont);
  FreeAndNil(FObjectFont);
  Application.HintHidePause := OldHintHidePause;
end;

procedure TFChartAdvEditor.ClearBuf;
var
  I: Integer;
begin
  for I := 0 to Buf.Count - 1 do
    if TBuferItem(Buf.Items[i]).ObjType = iConnection then
      TdxFcConnection(TBuferItem(Buf.Items[i]).Obj).Free;
  for i := 0 to Buf.Count - 1 do
    if TBuferItem(Buf.Items[i]).ObjType = iObject then
      TdxFcObject(TBuferItem(Buf.Items[i]).Obj).Free;
  while Buf.Count > 0 do
  begin
    TBuferItem(Buf.Items[0]).Free;
    Buf.Delete(0);
  end;
end;

procedure TFChartAdvEditor.ObjAssign(Source, Dest : TdxFcObject);
begin
  Dest.Font.Assign(Source.Font);
  Dest.ParentFont := Source.ParentFont;
  Dest.Text := Source.Text;
  Dest.BkColor := Source.BkColor;
  Dest.BorderStyle := Source.BorderStyle;
  Dest.CustomData := Source.CustomData;
  Dest.Data := Source.Data;
  Dest.EdgeStyle := Source.EdgeStyle;
  Dest.Height := Source.Height;
  Dest.HorzImagePos := Source.HorzImagePos;
  Dest.HorzTextPos := Source.HorzTextPos;
  Dest.ImageIndex := Source.ImageIndex;
  Dest.Left := Source.Left;
  Dest.ShapeColor := Source.ShapeColor;
  Dest.ShapeStyle := Source.ShapeStyle;
  Dest.ShapeType := Source.ShapeType;
  Dest.ShapeWidth := Source.ShapeWidth;
  Dest.Tag := Source.Tag;
  Dest.Top := Source.Top;
  Dest.Transparent := Source.Transparent;
  Dest.VertImagePos := Source.VertImagePos;
  Dest.VertTextPos := Source.VertTextPos;
  Dest.Visible := Source.Visible;
  Dest.Width := Source.Width;
  Dest.ZOrder := Source.ZOrder;
end;

procedure TFChartAdvEditor.ConAssign(Source, Dest : TdxFcConnection);
var
  I: Integer;
begin
  Dest.Font.Assign(Source.Font);
  Dest.ParentFont := Source.ParentFont;
  Dest.Text := Source.Text;
  Dest.ArrowSource.ArrowType := Source.ArrowSource.ArrowType;
  Dest.ArrowSource.Height := Source.ArrowSource.Height;
  Dest.ArrowSource.Width := Source.ArrowSource.Width;
  Dest.ArrowSource.Color := Source.ArrowSource.Color;
  Dest.ArrowDest.ArrowType := Source.ArrowDest.ArrowType;
  Dest.ArrowDest.Height := Source.ArrowDest.Height;
  Dest.ArrowDest.Width := Source.ArrowDest.Width;
  Dest.ArrowDest.Color := Source.ArrowDest.Color;
  Dest.Color := Source.Color;
  Dest.PenStyle := Source.PenStyle;
  for I := 0 to Source.PointCount - 1 do
    Dest.AddPoint(Source.Points[I]);
  Dest.Style := Source.Style;
  Dest.Transparent := Source.Transparent;
end;

procedure TFChartAdvEditor.CopyToBuf;
var
  I, J: Integer;
  ABufItem: TBuferItem;
  SObj, DObj, AObj: TdxFcObject;
  ACon: TdxFcConnection;
  List: TList;
begin
  List := TList.Create;
  try
    ClearBuf;
    for I := 0 to Chart.SelectedObjectCount - 1 do
    begin
      ABufItem := TBuferItem.Create;
      AObj := TdxFcObject.Create(BufChart);
      ObjAssign(Chart.SelectedObjects[i], AObj);
      ABufItem.Obj := AObj;
      ABufItem.ObjType := iObject;
      Buf.Add(ABufItem);
      List.Add(AObj);
    end;

    for I := 0 to Chart.SelectedConnectionCount - 1 do
    begin
      ABufItem := TBuferItem.Create;
      ACon := TdxFcConnection.Create(BufChart);
      ConAssign(Chart.SelectedConnections[i], ACon);
      SObj := nil;
      DObj := nil;

      for J := 0 to Chart.SelectedObjectCount - 1 do
      begin
        if Chart.SelectedConnections[i].ObjectSource = Chart.SelectedObjects[j] then
          SObj := TdxFcObject(List.Items[j]);
        if Chart.SelectedConnections[i].ObjectDest = Chart.SelectedObjects[j] then
          DObj := TdxFcObject(List.Items[j]);
      end;
      ACon.SetObjectSource(SObj, Chart.SelectedConnections[i].PointSource);
      ACon.SetObjectDest(DObj, Chart.SelectedConnections[i].PointDest);
      ABufItem.Obj := ACon;
      ABufItem.ObjType := iConnection;
      Buf.Add(ABufItem);
    end;
  finally
    List.Free;
  end;
end;

procedure TFChartAdvEditor.PasteFromBuf;
const
  D = 5;
var
  ACon: TdxFcConnection;
  I, J, Index: Integer;
  List : TList;
  P: TPoint;
  SObj, DObj, AObj: TdxFcObject;
begin
  FChange := False;

  List := TList.Create;
  try
    Chart.ClearSelection;
    for i := 0 to Buf.Count - 1 do
      if TBuferItem(Buf.Items[i]).ObjType = iObject then
      begin
        AObj := TdxFcObject.Create(Chart);
        ObjAssign(TdxFcObject(TBuferItem(Buf.Items[i]).Obj), AObj);
        AObj.Left := AObj.Left + d;
        AObj.Top := AObj.Top + d;
        AObj.BringToFront;
        AObj.Selected := True;
        List.Add(AObj);
      end;

    for i := 0 to Buf.Count - 1 do
      if TBuferItem(Buf.Items[i]).ObjType = iConnection then
      begin
        ACon := TdxFcConnection.Create(Chart);
        ConAssign(TdxFcConnection(TBuferItem(Buf.Items[i]).Obj), ACon);
        for j := 0 to ACon.PointCount - 1 do
        begin
          P := ACon.Points[j];
          P.X := P.X + d;
          P.Y := P.Y + d;
          ACon.Points[j] := P;
        end;
        SObj := nil;
        DObj := nil;
        Index := -1;
        for j := 0 to Buf.Count - 1 do
          if TBuferItem(Buf.Items[j]).ObjType = iObject then
          begin
            inc(Index);
            if TdxFcObject(TBuferItem(Buf.Items[j]).Obj) = TdxFcConnection(TBuferItem(Buf.Items[i]).Obj).ObjectSource then
              SObj := TdxFcObject(List.Items[Index]);
            if TdxFcObject(TBuferItem(Buf.Items[j]).Obj) = TdxFcConnection(TBuferItem(Buf.Items[i]).Obj).ObjectDest then
              DObj := TdxFcObject(List.Items[Index]);
          end;
        ACon.SetObjectSource(SObj, TdxFcConnection(TBuferItem(Buf.Items[i]).Obj).PointSource);
        ACon.SetObjectDest(DObj, TdxFcConnection(TBuferItem(Buf.Items[i]).Obj).PointDest);
        ACon.Selected := True;
      end;
  finally
    List.Free;
  end;
  FChange := True;
end;

procedure TFChartAdvEditor.iCopyClick(Sender: TObject);
begin
  CopyToBuf;
  ChartSelected(Chart, nil);
end;

procedure TFChartAdvEditor.iPasteClick(Sender: TObject);
begin
  FStore := False;
  PasteFromBuf;
  FStore := True;
  ChartChange(Chart, nil);
end;

procedure TFChartAdvEditor.iCutClick(Sender: TObject);
begin
  FStore := False;
  CopyToBuf;
  Chart.DeleteSelection;
  FStore := True;
  ChartChange(Chart, nil);
end;

procedure TFChartAdvEditor.iEditClick(Sender: TObject);
begin
  ChartDblClick(Chart);
end;

procedure TFChartAdvEditor.ChartChange(Sender: TdxCustomFlowChart;
  Item: TdxFcItem);
begin
  if Self.HandleAllocated then
  begin
    ChartSelected(Chart, nil);
    if FStore then
    begin
      FUndo.Store;
      miUndo.Enabled := FUndo.CanUndo;
    end;
  end;
end;

procedure TFChartAdvEditor.FormShow(Sender: TObject);
begin
  ChartChange(Chart, nil);
end;

procedure TFChartAdvEditor.DrawDrag(P1, P2 : TPoint; Mode : Integer);
var
  AMode: TPenMode;
  AColor: TColor;
  ABkColor: TColor;
  AStyle: TPenStyle;
  PP1 , PP2: TPoint;
begin
  if P1.X < P2.X then
  begin
    PP1.X := P1.X;
    PP2.X := P2.X;
  end
  else
  begin
    PP1.X := P2.X;
    PP2.X := P1.X;
  end;

  if P1.Y < P2.Y then
  begin
    PP1.Y := P1.Y;
    PP2.Y := P2.Y;
  end
  else
  begin
    PP1.Y := P2.Y;
    PP2.Y := P1.Y;
  end;

  with TChart(Chart).Canvas.Canvas do
  begin
    AMode := Pen.Mode;
    AColor := Pen.Color;
    AStyle := Pen.Style;
    ABkColor := Brush.Color;
    Pen.Mode := pmNotXor;
    Pen.Color := clBlack;
    if Mode = 3 then
      Pen.Style := psDot;
    Brush.Style := bsClear;
    if Mode = 1 then
      case sbShape.Tag of
      0, 1 : Rectangle(PP1.X, PP1.Y, PP2.X, PP2.Y);
         2 : Ellipse(PP1.X, PP1.Y, PP2.X, PP2.Y);
         3 : RoundRect(PP1.X, PP1.Y, PP2.X, PP2.Y, (PP2.X - PP1.X) shr 1, (PP2.Y - PP1.Y) shr 1);
         4 : PolyLine([Point(PP1.X + (PP2.X - PP1.X) shr 1, PP1.Y),
                       Point(PP2.X, PP1.Y + (PP2.Y - PP1.Y) shr 1),
                       Point(PP1.X + (PP2.X - PP1.X) shr 1, PP2.Y),
                       Point(PP1.X, PP1.Y + (PP2.Y - PP1.Y) shr 1),
                       Point(PP1.X + (PP2.X - PP1.X) shr 1, PP1.Y)]);
         5 : PolyLine([Point(PP1.X + (PP2.X - PP1.X) shr 1, PP1.Y),
                       Point(PP2.X, PP2.Y),
                       Point(PP1.X, PP2.Y),
                       Point(PP1.X + (PP2.X - PP1.X) shr 1, PP1.Y)]);
         6 : PolyLine([Point(PP1.X, PP1.Y),
                       Point(PP2.X, PP1.Y),
                       Point(PP1.X + (PP2.X - PP1.X) shr 1, PP2.Y),
                       Point(PP1.X, PP1.Y)]);
         7 : PolyLine([Point(PP1.X, PP1.Y),
                       Point(PP2.X,  PP1.Y + (PP2.Y - PP1.Y) shr 1),
                       Point(PP1.X, PP2.Y),
                       Point(PP1.X, PP1.Y)]);
         8 : PolyLine([Point(PP1.X, PP1.Y + (PP2.Y - PP1.Y) shr 1),
                       Point(PP2.X, PP1.Y),
                       Point(PP2.X, PP2.Y),
                       Point(PP1.X, PP1.Y + (PP2.Y - PP1.Y) shr 1)]);
         9 : PolyLine([Point(PP1.X + (PP2.X - PP1.X) shr 2, PP1.Y),
                       Point(PP2.X - (PP2.X - PP1.X) shr 2, PP1.Y),
                       Point(PP2.X, PP1.Y + (PP2.Y - PP1.Y) shr 1),
                       Point(PP2.X - (PP2.X - PP1.X) shr 2, PP2.Y),
                       Point(PP1.X + (PP2.X - PP1.X) shr 2, PP2.Y),
                       Point(PP1.X, PP1.Y + (PP2.Y - PP1.Y) shr 1),
                       Point(PP1.X + (PP2.X - PP1.X) shr 2, PP1.Y)]);
    end;
    if Mode = 2 then
    begin
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P2.Y);
    end;
    if Mode = 3 then
      Rectangle(P1.X, P1.Y, P2.X, P2.Y);
    Pen.Mode := AMode;
    Pen.Color := AColor;
    Pen.Style := AStyle;
    Brush.Color := ABkColor;
  end;
end;

procedure TFChartAdvEditor.iRemovePointClick(Sender: TObject);
var
  I, J, B, K: Integer;
  P1 : TPoint;
begin
  P1 := DownPoint;
  with Chart do
    for i := 0 to SelectedConnectionCount - 1 do
      with SelectedConnections[i] do
      begin
        j := GetNearestPoint(P1.X, P1.Y);
        b := 1;
        k := PointCount - 2;
        if ObjectSource <> nil then dec(b);
        if ObjectDest <> nil then inc(K);
        if (j >= b) and (j <= k) then
           RemovePoint(j);
      end;
end;

procedure TFChartAdvEditor.ChartPopupMenuPopup(Sender: TObject);
var
  I, J, B, K: Integer;
  P1 : TPoint;
begin
  with Chart do
  begin
    if SelectedConnectionCount > 0 then
    begin
      P1 := DownPoint;
      iRemovePoint.Enabled := False;
      if GetHitTestAt(P1.X, P1.Y) * [htOnSelPoint] <> [] then
      with Chart do
        for i := 0 to SelectedConnectionCount - 1 do
          with SelectedConnections[i] do
          begin
            j := GetNearestPoint(P1.X, P1.Y);
            b := 1;
            k := PointCount - 2;
            if ObjectSource <> nil then dec(b);
            if ObjectDest <> nil then inc(K);
            if (j >= b) and (j <= k) then
            begin
              iRemovePoint.Enabled := True;
              break;
            end;
          end;
    end
    else
      iRemovePoint.Enabled := False;
  end;
end;

procedure TFChartAdvEditor.ChartKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
var
  P: TPoint;
begin
  if (Key = VK_MENU) and sbZoom.Down then
    Chart.Cursor := crFlChartZoomOut;
  if Key = VK_APPS then
  begin
    GetCursorPos(P);
    P := Chart.ScreenToClient(P);
    DownPoint := Point(P.X, P.Y);
    GetCursorPos(P);
    bpmChart.Popup(P.X, P.Y);
  end;
  if (Key = VK_INSERT) and (ssCtrl in Shift) and iCopy.Enabled then iCopyClick(iCopy);
  if (Key = VK_INSERT) and (ssShift in Shift) and iPaste.Enabled then iPasteClick(iPaste);
  if (Key = VK_DELETE) and (ssShift in Shift) and iCut.Enabled then iCutClick(iCut);

  if FNewObject then
  begin
    if btnCreateObject.Down then
    begin
      GetCursorPos(P);
      P := Chart.ScreenToClient(P);
      if (Abs(DownPoint.X - P.X) > 5) and (Abs(DownPoint.Y - P.Y) > 5) and (Key <> VK_ESCAPE)then
        ChartMouseUp(Chart, mbLeft, [], P.X, P.Y)
      else
      begin
        btnCreateObject.Down := False;
        DrawDrag(DownPoint, OldPoint, 1);
        FNewObject := False;
        ClipCursor(nil);
      end;
    end;
    if btnCreateConnect.Down then
    begin
      GetCursorPos(P);
      P := Chart.ScreenToClient(P);
      if ((Abs(DownPoint.X - P.X) > 5) or (Abs(DownPoint.Y - P.Y) > 5)) and (Key <> VK_ESCAPE) then
        ChartMouseUp(Chart, mbLeft, [], P.X, P.Y)
      else
      begin
        btnCreateConnect.Down := False;
        DrawDrag(DownPoint, OldPoint, 2);
        FNewObject := False;
        ClipCursor(nil);
      end;
    end;
  end;

end;

procedure TFChartAdvEditor.ChartKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
  begin
    if sbZoom.Down then
      Chart.Cursor := crFlChartZoomIn
    else
      Chart.Cursor := crDefault;
  end;
  if Key = VK_DELETE then
    ChartChange(Chart, nil);
end;

procedure TFChartAdvEditor.miUndoClick(Sender: TObject);
begin
  FStore := False;
  FUndo.Undo;
  miUndo.Enabled := FUndo.CanUndo;
  FStore := True;
end;

function TFChartAdvEditor.IsMainItemInUnion(Obj : TdxFcObject) : Boolean;
begin
  Result := Obj.IsUnion;
end;

function TFChartAdvEditor.GetMainItemInUnion(AChart : TdxFlowChart; Obj : TdxFcObject) : TdxFcObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AChart.ObjectCount - 1 do
    if AChart.Objects[I].IsUnion then
      if AChart.Objects[I].HasInUnion(Obj) then
      begin
        Result := AChart.Objects[I];
        Break;
      end;
end;

function TFChartAdvEditor.GetUnionByNumber(AChart : TdxFlowChart; Number : Integer) : TdxFcObject;
var
  I, Count: Integer;
begin
  Count := 0;
  Result := nil;
  for I := 0 to AChart.ObjectCount - 1 do
    if AChart.Objects[i].IsUnion then
    begin
      Inc(Count);
      if Count = Number then
      begin
        Result := AChart.Objects[i];
        break;
      end;
    end;
end;

function TFChartAdvEditor.ChartHasUnions(AChart : TdxFlowChart) : Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AChart.ObjectCount - 1 do
    if AChart.Objects[I].IsUnion then
    begin
      Result := True;
      Break;
    end;
end;

procedure TFChartAdvEditor.iClearAllUnionsClick(Sender: TObject);
var
  I: Integer;
  FChange: Boolean;
begin
  FChange := False;
  for I := 0 to Chart.ObjectCount - 1 do
    if Chart.Objects[i].IsUnion then
    begin
      Chart.Objects[i].ClearUnion;
      FChange := True;
    end;

  if FChange then
    FUndo.Store;
end;

procedure TFChartAdvEditor.iClearUnionClick(Sender: TObject);
var
  I: Integer;
  FChange : Boolean;
begin
  FChange := False;
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if Chart.SelectedObjects[I].IsUnion then
    begin
      Chart.SelectedObjects[I].ClearUnion;
      FChange := True;
    end;

  if FChange then
    FUndo.Store;
end;

procedure TFChartAdvEditor.iNewUnionClick(Sender: TObject);
var
  I: Integer;
  MainObj, FirstObj: TdxFcObject;
  FFind, FChange: Boolean;
begin
  MainObj := nil;
  FChange := False;
  if Chart.SelectedObjectCount > 0 then
    MainObj := Chart.SelectedObjects[0];
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if MainObj <> nil then
    begin
      FFind := False;
      FirstObj := nil;
      repeat
        FirstObj := FindUnions(Chart, FirstObj, MainObj);
        if FirstObj = Chart.SelectedObjects[i] then
        begin
          FFind := True;
          break;
        end;
      until FirstObj = nil;
      if not FFind then
      begin
        MainObj.AddToUnion(Chart.SelectedObjects[i]);
        FChange := True;
      end;
    end;
  if FChange then
    FUndo.Store;
end;

procedure TFChartAdvEditor.iAddToUnionClick(Sender: TObject);
var
  FFind, FChange: Boolean;
  I: Integer;
  Obj, FirstObj: TdxFcObject;
begin
  FChange := False;
  if ChartHasUnions(Chart) then
  begin
    Obj := SelectUnion(Chart, nil);
    if Obj <> nil then
      for i := 0 to Chart.SelectedObjectCount - 1 do
      begin
        FirstObj := nil;
        FFind := False;
        repeat
          FirstObj := FindUnions(Chart, FirstObj, Obj);
          if FirstObj = Chart.SelectedObjects[i] then
          begin
            FFind := True;
            break;
          end;
        until FirstObj = nil;
        if not FFind then
        begin
          Obj.AddToUnion(Chart.SelectedObjects[i]);
          FChange := True;
        end;
      end;
  end;
  if FChange then FUndo.Store;
end;

procedure TFChartAdvEditor.iRemoveFromUnionClick(Sender: TObject);
var
  FFind, FChange: Boolean;
  I: integer;
  Obj: TdxFcObject;
begin
  FFind := False;
  FChange := False;
  for I := 0 to Chart.SelectedObjectCount - 1 do
    if IsChildItemInUnion(Chart, Chart.SelectedObjects[i]) then
      FFind := True;

  if FFind then
  begin
    Obj := SelectUnion(Chart, Chart.SelectedObjects[0]);
    if Obj <> nil then
    begin
      for i := 0 to Chart.SelectedObjectCount - 1 do
        Obj.RemoveFromUnion(Chart.SelectedObjects[i]);
      FChange := True;
    end;
  end;
  if FChange then FUndo.Store;
end;

function TFChartAdvEditor.GetImageIndexByMenuItem(Item: TdxBarItem) : Integer;
begin
  Result := - 1;
  if Item = miOpen then begin Result := 0; Exit; end;
  if Item = miSaveAs then begin Result := 1; Exit; end;
  if Item = miUndo then begin Result := 2; Exit; end;
  if Item = miCut then begin Result := 3; Exit; end;
  if Item = miCopy then begin Result := 4;  Exit; end;
  if Item = miPaste then begin Result := 5; Exit; end;
  if Item = miDelete then begin Result := 6; Exit; end;
  if Item = miSelectAll then begin Result := 7; Exit; end;
  if Item = miBringToFront then begin Result := 8; Exit; end;
  if Item = miSendToBack then begin Result := 9; Exit; end;
  if Item = miNewUnion then begin Result := 10; Exit; end;
  if Item = miAddToUnion then begin Result := 11; Exit; end;
  if Item = miRemoveFromUnion then begin Result := 12; Exit; end;
  if Item = iNone then begin Result := 75; Exit; end;
  if Item = iRectangle then begin Result := 0; Exit; end;
  if Item = iEllipse then begin Result := 1; Exit; end;
  if Item = iRoundRect then begin Result := 2; Exit; end;
  if Item = iDiamond then begin Result := 3; Exit; end;
  if Item = iNorthTriangle then begin Result := 4; Exit; end;
  if Item = itSouthTriangle then begin Result := 5; Exit; end;
  if Item = itEastTriangle then begin Result := 6; Exit; end;
  if Item = itWestTriangle then begin Result := 7; Exit; end;
  if Item = itHexagon then begin Result := 8; Exit; end;
  if Item = i1p then begin Result := 9; Exit; end;
  if Item = i2p then begin Result := 10; Exit; end;
  if Item = i3p then begin Result := 11; Exit; end;
  if Item = i4p then begin Result := 12; Exit; end;
  if Item = i5p then begin Result := 13; Exit; end;
  if Item = i6p then begin Result := 14; Exit; end;
  if Item = i7p then begin Result := 15; Exit; end;
  if Item = i8p then begin Result := 16; Exit; end;
  if Item = i9p then begin Result := 17; Exit; end;
  if Item = i10p then begin Result := 18; Exit; end;
  if Item = iStraight then begin Result := 19; Exit; end;
  if Item = iCurved then begin Result := 20; Exit; end;
  if Item = iRectHorizontal then begin Result := 21; Exit; end;
  if Item = iRectVertical then begin Result := 22; Exit; end;
  if Item = iSNone then begin Result := 23; Exit; end;
  if Item = iSArrow then begin Result := 24; Exit; end;
  if Item = iSOvalArrow then begin Result := 25; Exit; end;
  if Item = iSRectArrow then begin Result := 26; Exit; end;
  if Item = iDNone then begin Result := 27; Exit; end;
  if Item = iDArrow then begin Result := 28; Exit; end;
  if Item = iDOvalArrow then begin Result := 29; Exit; end;
  if Item = iDRectArrow then begin Result := 30; Exit; end;
  if Item = iS10 then begin Result := 31; Exit; end;
  if Item = iS20 then begin Result := 32; Exit; end;
  if Item = iS30 then begin Result := 33; Exit; end;
  if Item = iS40 then begin Result := 34; Exit; end;
  if Item = iS50 then begin Result := 35; Exit; end;
  if Item = iD10 then begin Result := 36; Exit; end;
  if Item = iD20 then begin Result := 37; Exit; end;
  if Item = iD30 then begin Result := 38; Exit; end;
  if Item = iD40 then begin Result := 39; Exit; end;
  if Item = iD50 then begin Result := 40; Exit; end;
  if Item = iSP1 then begin Result := 41; Exit; end;
  if Item = iSP2 then begin Result := 42; Exit; end;
  if Item = iSP3 then begin Result := 43; Exit; end;
  if Item = iSP4 then begin Result := 44; Exit; end;
  if Item = iSP5 then begin Result := 45; Exit; end;
  if Item = iSP6 then begin Result := 46; Exit; end;
  if Item = iSP7 then begin Result := 47; Exit; end;
  if Item = iSP8 then begin Result := 48; Exit; end;
  if Item = iSP9 then begin Result := 49; Exit; end;
  if Item = iSP10 then begin Result := 50; Exit; end;
  if Item = iSP11 then begin Result := 51; Exit; end;
  if Item = iSP12 then begin Result := 52; Exit; end;
  if Item = iSP13 then begin Result := 53; Exit; end;
  if Item = iSP14 then begin Result := 54; Exit; end;
  if Item = iSP15 then begin Result := 55; Exit; end;
  if Item = iSP16 then begin Result := 56; Exit; end;
  if Item = iDP1 then begin Result := 41; Exit; end;
  if Item = iDP2 then begin Result := 42; Exit; end;
  if Item = iDP3 then begin Result := 43; Exit; end;
  if Item = iDP4 then begin Result := 44; Exit; end;
  if Item = iDP5 then begin Result := 45; Exit; end;
  if Item = iDP6 then begin Result := 46; Exit; end;
  if Item = iDP7 then begin Result := 47; Exit; end;
  if Item = iDP8 then begin Result := 48; Exit; end;
  if Item = iDP9 then begin Result := 49; Exit; end;
  if Item = iDP10 then begin Result := 50; Exit; end;
  if Item = iDP11 then begin Result := 51; Exit; end;
  if Item = iDP12 then begin Result := 52; Exit; end;
  if Item = iDP13 then begin Result := 53; Exit; end;
  if Item = iDP14 then begin Result := 54; Exit; end;
  if Item = iDP15 then begin Result := 55; Exit; end;
  if Item = iDP16 then begin Result := 56; Exit; end;
  if Item = iTextTopLeft then begin Result := 57; Exit; end;
  if Item = iTextTop then begin Result := 58; Exit; end;
  if Item = iTextTopRight then begin Result := 59; Exit; end;
  if Item = iTextLeft then begin Result := 60; Exit; end;
  if Item = iTextCenter then begin Result := 61; Exit; end;
  if Item = iTextRight then begin Result := 62; Exit; end;
  if Item = iTextBottomLeft then begin Result := 63; Exit; end;
  if Item = iTextBottom then begin Result := 64; Exit; end;
  if Item = iTextBottomRight then begin Result := 65; Exit; end;
  if Item = iImageTopLeft then begin Result := 66; Exit; end;
  if Item = iImageTop then begin Result := 67; Exit; end;
  if Item = iImageTopRight then begin Result := 68; Exit; end;
  if Item = iImageLeft then begin Result := 69; Exit; end;
  if Item = iImageCenter then begin Result := 70; Exit; end;
  if Item = iImageRight then begin Result := 71; Exit; end;
  if Item = iImageBottomLeft then begin Result := 72; Exit; end;
  if Item = iImageBottom then begin Result := 73; Exit; end;
  if Item = iImageBottomRight then begin Result := 74; Exit; end;
  if Item = iCut then begin Result := 3; Exit; end;
  if Item = iCopy then begin Result := 4; Exit; end;
  if Item = iPaste then begin Result := 5; Exit; end;
  if Item = iDelete then begin Result := 6; Exit; end;
  if Item = iSelectAll then begin Result := 7; Exit; end;
  if Item = iBringToFront then begin Result := 8; Exit; end;
  if Item = iSendToBack then begin Result := 9; Exit; end;
  if Item = iNewUnion then begin Result := 10; Exit; end;
  if Item = iAddToUnion then begin Result := 11; Exit; end;
  if Item = iRemoveFromUnion then begin Result := 12; Exit; end;
  if Item = miZoomIn then begin Result := 13; Exit; end;
  if Item = miZoomOut then begin Result := 14; Exit; end;
  if Item = miFit then begin Result := 15; Exit; end;
end;

procedure TFChartAdvEditor.miZoomInClick(Sender: TObject);
begin
  if Chart.Zoom < 490 then
    Chart.Zoom := Chart.Zoom + 10
  else
    miZoomIn.Enabled := False;

  miZoomOut.Enabled := True;
  miActualSize.Enabled := (Chart.Zoom <> 100) and (not sbFit.Down);
end;

procedure TFChartAdvEditor.miZoomOutClick(Sender: TObject);
begin
  if Chart.Zoom > 20 then
    Chart.Zoom := Chart.Zoom - 10
  else
    miZoomOut.Enabled := False;

  miZoomIn.Enabled := True;
  miActualSize.Enabled := (Chart.Zoom <> 100) and (not sbFit.Down);
end;

procedure TFChartAdvEditor.miFitClick(Sender: TObject);
begin
  miFit.Down := not miFit.Down;
  if miFit.Down then
  begin
    miZoomIn.Enabled := False;
    miZoomOut.Enabled := False;
    Chart.Zoom := 0;
    sbZoom.Enabled := False;
    sbFit.Down := True;
    Chart.Cursor := crDefault;
  end
  else
  begin
    miZoomIn.Enabled := True;
    miZoomOut.Enabled := True;
    Chart.Zoom := 100;
    sbZoom.Enabled := True;
    sbFit.Down := False;
  end;
  miActualSize.Enabled := (Chart.Zoom <> 100) and (not sbFit.Down);
end;

procedure TFChartAdvEditor.miActualSizeClick(Sender: TObject);
begin
  Chart.Zoom := 100;
end;

procedure TFChartAdvEditor.miAntialiasingClick(Sender: TObject);
begin
  Chart.Antialiasing := miAntialiasing.Down;
end;

procedure TFChartAdvEditor.miDynamicMovingClick(Sender: TObject);
begin
  if miDynamicMoving.Down then
    Chart.Options := Chart.Options + [fcoDynamicMoving]
  else
    Chart.Options := Chart.Options - [fcoDynamicMoving];
end;

procedure TFChartAdvEditor.miDynamicSizingClick(Sender: TObject);
begin
  if miDynamicSizing.Down then
    Chart.Options := Chart.Options + [fcoDynamicSizing]
  else
    Chart.Options := Chart.Options - [fcoDynamicSizing];
end;

function TFChartAdvEditor.Execute(AChart: TdxFlowChart): Boolean;
var
  ASavedContent: TMemoryStream;
  ASavedEvent: TdxFcEvent;
  P : TPoint;
begin
  Result := False;
  ASavedEvent := AChart.OnDeletion;
  AChart.OnDeletion := nil;
  ASavedContent := TMemoryStream.Create;
  try
    AChart.SaveToStream(ASavedContent);
    ASavedContent.Position := 0;

    miAntialiasing.Down := AChart.Antialiasing;
    Chart.Antialiasing := AChart.Antialiasing;
    Chart.LookAndFeel.MasterLookAndFeel := AChart.LookAndFeel;
    Chart.Images := AChart.Images;
    Chart.Color := AChart.Color;
    Chart.Font := AChart.Font;
    Chart.Font.Height := ScaleFactor.Apply(AChart.Font.Height, TdxFlowChartAccess(AChart).ScaleFactor);
    Chart.Zoom := 100;
    Chart.LoadFromStream(ASavedContent);

    if ShowModal = mrOK then
    begin
      ASavedContent.Position := 0;
      Chart.SaveToStream(ASavedContent);
      ASavedContent.Position := 0;
      AChart.LoadFromStream(ASavedContent);
      AChart.Color := Chart.Color;
      Result := True;
    end;
  finally
    ASavedContent.Free;
    AChart.OnDeletion := ASavedEvent;
  end;
  GetCursorPos(P);
  PostMessage(AChart.Handle, WM_LBUTTONUP, 0, MakeLParam(P.x, P.y));
end;

function TFChartAdvEditor.CanKeyEnter(Edit: TcxTextEdit; Key: Char; MinValue, MaxValue: Integer): Boolean;
var
  Text: string;
  Value: Integer;
begin
  if Key >= #32 then
  begin
    if Edit.SelLength = 0 then
      Text := Edit.Text + Key
    else
      Text := Copy(Edit.Text, 1, Edit.SelStart - 1) + Key +
        Copy(Edit.Text, Edit.SelStart + Edit.SelLength,
        Length(Edit.Text) - Edit.SelStart - Edit.SelLength + 1);
  end
  else
    Text := Edit.Text;

  try
    if Text = '' then
      Value := 1
    else
      Value := StrToInt(Text);

    Result := (Value >= MinValue) and (Value <= MaxValue);
  except
    Result := False;
  end;
end;

procedure TFChartAdvEditor.InitializeMenuImageIndexes;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[i] is TdxBarItem then
      TdxBarItem(Components[i]).ImageIndex := GetImageIndexByMenuItem(TdxBarItem(Components[i]));
end;

procedure TFChartAdvEditor.LoadLocalizations;
begin
  btnCancel.Caption := cxGetResourceString(@sdxFlowChartDialogButtonCancel);
  btnOK.Caption := cxGetResourceString(@sdxFlowChartDialogButtonOk);

  miFile.Caption := cxGetResourceString(@sdxFlowChartEditorFile);
  miOpen.Caption := cxGetResourceString(@sdxFlowChartEditorFileOpen);
  miSaveAs.Caption := cxGetResourceString(@sdxFlowChartEditorFileSave);

  miBringToFront.Caption := cxGetResourceString(@sdxFlowChartEditorEditBringToFront);
  miClearSelection.Caption := cxGetResourceString(@sdxFlowChartEditorEditClearSelection);
  miCopy.Caption := cxGetResourceString(@sdxFlowChartEditorEditCopy);
  miCut.Caption := cxGetResourceString(@sdxFlowChartEditorEditCut);
  miDelete.Caption := cxGetResourceString(@sdxFlowChartEditorEditDelete);
  miEdit.Caption := cxGetResourceString(@sdxFlowChartEditorEdit);
  miPaste.Caption := cxGetResourceString(@sdxFlowChartEditorEditPaste);
  miSelectAll.Caption := cxGetResourceString(@sdxFlowChartEditorEditSelectAll);
  miSendToBack.Caption := cxGetResourceString(@sdxFlowChartEditorEditSendToBack);
  miUndo.Caption := cxGetResourceString(@sdxFlowChartEditorEditUndo);

  iAddToUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsAdd);
  iBringToFront.Caption := cxGetResourceString(@sdxFlowChartEditorEditBringToFront);
  iClearAllUnions.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsClearAll);
  iClearSelection.Caption := cxGetResourceString(@sdxFlowChartEditorEditClearSelection);
  iClearUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsClear);
  iCopy.Caption := cxGetResourceString(@sdxFlowChartEditorEditCopy);
  iCut.Caption := cxGetResourceString(@sdxFlowChartEditorEditCut);
  iDelete.Caption := cxGetResourceString(@sdxFlowChartEditorEditDelete);
  iEdit.Caption := cxGetResourceString(@sdxFlowChartEditorProperties);
  iNewUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsNew);
  iPaste.Caption := cxGetResourceString(@sdxFlowChartEditorEditPaste);
  iRemoveFromUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsRemove);
  iRemovePoint.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsRemove);
  iSelectAll.Caption := cxGetResourceString(@sdxFlowChartEditorEditSelectAll);
  iSendToBack.Caption := cxGetResourceString(@sdxFlowChartEditorEditSendToBack);

  miActualSize.Caption := cxGetResourceString(@sdxFlowChartEditorViewActualSize);
  miAntialiasing.Caption := cxGetResourceString(@sdxFlowChartEditorViewAntialiasing);
  miFit.Caption := cxGetResourceString(@sdxFlowChartEditorViewFit);
  miZoomIn.Caption := cxGetResourceString(@sdxFlowChartEditorViewZoomIn);
  miZoomOut.Caption := cxGetResourceString(@sdxFlowChartEditorViewZoomOut);
  sbFit.Hint := cxGetResourceString(@sdxFlowChartEditorFitHint);
  sbZoom.Hint := cxGetResourceString(@sdxFlowChartEditorZoomHint);
  View1.Caption := cxGetResourceString(@sdxFlowChartEditorView);

  btnCreateConnect.Hint := cxGetResourceString(@sdxFlowChartEditorCreateConnectionHint);
  btnCreateObject.Hint := cxGetResourceString(@sdxFlowChartEditorCreateObjectHint);
  Label1.Caption := cxGetResourceString(@sdxFlowChartEditorCreate);
  Label2.Caption := cxGetResourceString(@sdxFlowChartEditorObject);
  sbShape.Hint := cxGetResourceString(@sdxFlowChartEditorObjectShapeStyleHint);
  sbLine.Hint := cxGetResourceString(@sdxFlowChartEditorObjectLineWidthHint);
  sbTextPosition.Hint := cxGetResourceString(@sdxFlowChartEditorObjectTextPositionHint);
  sbImagePosition.Hint := cxGetResourceString(@sdxFlowChartEditorObjectImagePositionHint);
  sbObjectFont.Hint := cxGetResourceString(@sdxFlowChartEditorObjectTextFontHint);
  Label5.Caption := cxGetResourceString(@sdxFlowChartEditorConnection);
  sbStyle.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionStyleHint);
  sbConnectFont.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionTextFontHint);
  sbSourceArrow.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionArrowSourceHint);
  sbDestArrow.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionArrowDestinationHint);
  sbDArrowSize.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionArrowDestinationSizeHint);
  sbSArrowSize.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionArrowSourceSizeHint);
  sbDPoint.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionLinkedPointDestinationHint);
  sbSPoint.Hint := cxGetResourceString(@sdxFlowChartEditorConnectionLinkedPointSourceHint);

  miAddToUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsAdd);
  miClearAllUnions.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsClearAll);
  miClearUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsClear);
  miNewUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsNew);
  miRemoveFromUnion.Caption := cxGetResourceString(@sdxFlowChartEditorUnionsRemove);
  miUnions.Caption := cxGetResourceString(@sdxFlowChartEditorUnions);

  miOptions.Caption := cxGetResourceString(@sdxFlowChartEditorOptions);
  miDynamicMoving.Caption := cxGetResourceString(@sdxFlowChartEditorOptionsDynamicMoving);
  miDynamicSizing.Caption := cxGetResourceString(@sdxFlowChartEditorOptionsDynamicSizing);
  miHelp.Caption := cxGetResourceString(@sdxFlowChartEditorHelp);
  Contents1.Caption := cxGetResourceString(@sdxFlowChartEditorHelpContents);

  LocalizeShapeMenu;
  LocalizeStyleMenu;
  LocalizeArrowStyleMenu(bpmSourceArrow);
  LocalizeArrowStyleMenu(bpmDestArrow);
  LocalizeIndexesMenu(bpmLine, cxGetResourceString(@sdxFlowChartEditorPixels));
  LocalizeIndexesMenu(bpmSPoint, cxGetResourceString(@sdxFlowChartEditorPoint));
  LocalizeIndexesMenu(bpmDPoint, cxGetResourceString(@sdxFlowChartEditorPoint));
  LocalizeLayoutMenu(bpmTextPosition);
  LocalizeLayoutMenu(bpmImagePosition);
end;

procedure TFChartAdvEditor.LocalizeArrowStyleMenu(AMenu: TdxBarPopupMenu);
var
  AItem: TdxBarItem;
  I: Integer;
begin
  for I := 0 to AMenu.ItemLinks.Count - 1 do
  begin
    AItem := AMenu.ItemLinks[I].Item;
    AItem.Caption := cxGetResourceString(dxFlowChartArrowStyleNamesMap[TdxFcaType(AItem.Tag)]);
  end;
end;

procedure TFChartAdvEditor.LocalizeIndexesMenu(AMenu: TdxBarPopupMenu; const AFormatString: string);
var
  AItem: TdxBarItem;
  I: Integer;
begin
  for I := 0 to AMenu.ItemLinks.Count - 1 do
  begin
    AItem := AMenu.ItemLinks[I].Item;
    AItem.Caption := Format(AFormatString, [AItem.Tag]);
  end;
end;

procedure TFChartAdvEditor.LocalizeLayoutMenu(AMenu: TdxBarPopupMenu);
var
  I, ACount: Integer;
begin
  ACount := Min(AMenu.ItemLinks.Count, Length(dxFlowChartLayoutNamesMap));
  for I := 0 to ACount - 1 do
    AMenu.ItemLinks[I].Item.Caption := cxGetResourceString(dxFlowChartLayoutNamesMap[I]);
end;

procedure TFChartAdvEditor.LocalizeStyleMenu;
var
  AItem: TdxBarItem;
  I: Integer;
begin
  for I := 0 to bpmStyle.ItemLinks.Count - 1 do
  begin
    AItem := bpmStyle.ItemLinks[I].Item;
    AItem.Caption := cxGetResourceString(dxFlowChartConnectionStyleNamesMap[TdxFclStyle(AItem.Tag - 1)]);
  end;
end;

procedure TFChartAdvEditor.LocalizeShapeMenu;
var
  AItem: TdxBarItem;
  I: Integer;
begin
  for I := 0 to bpmShape.ItemLinks.Count - 1 do
  begin
    AItem := bpmShape.ItemLinks[I].Item;
    AItem.Caption := cxGetResourceString(dxFlowChartShapeNamesMap[TdxFcShapeType(AItem.Tag)]);
  end;
end;

procedure TFChartAdvEditor.btnCreateObjectClick(Sender: TObject);
begin
  if TcxButton(Sender).Down then
  begin
    Chart.ClearSelection;
    Chart.Options := Chart.Options - [fcoMultiSelect];
  end;
end;

procedure TFChartAdvEditor.Contents1Click(Sender: TObject);

  function TryOpen(const AFileName: string): Boolean;
  begin
    Result := FileExists(AFileName) and dxShellExecute(Handle, AFileName, SW_SHOWMAXIMIZED);
  end;

const
  sHelp = 'ExpressFlowChartEditor.chm';
begin
  if TryOpen(sHelp) then Exit;
  TryOpen(ExtractFilePath(Application.HelpFile) + sHelp);
end;

initialization
  dxFlowChartCustomizeFormManager.Register(TFChartAdvEditor);

  Screen.Cursors[crFlChartZoomIn] := LoadCursor(HInstance, 'ZOOMIN');
  Screen.Cursors[crFlChartZoomOut] := LoadCursor(HInstance, 'ZOOMOUT');

finalization
  dxFlowChartCustomizeFormManager.Unregister(TFChartAdvEditor);

end.
