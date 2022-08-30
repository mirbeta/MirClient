{=======================================================================================================================
  DisplayFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit DisplayFrame;

interface

uses
  Forms,
  RzStatus,
  ImgList,
  Controls,
  RzPanel,
  RzEdit,
  RzBorder,
  RzAnimtr,
  RzRadGrp,
  RzButton,
  RzRadChk,
  StdCtrls,
  RzCmboBx,
  Mask,
  RzSpnEdt,
  RzTrkBar,
  RzLabel,
  RzPrgres,
  Classes,
  ExtCtrls,
  Windows,
  Graphics,
  RzBckgnd,
  RzCommon;

type
  TFmeDisplay = class(TFrame)
    RzPanel1: TRzPanel;
    RzBackground1: TRzBackground;
    GrpTRzProgressBar: TRzGroupBox;
    PbrPreview: TRzProgressBar;
    RzLabel2: TRzLabel;
    TrkPercent: TRzTrackBar;
    SpnTotal: TRzSpinEdit;
    SpnPartsComp: TRzSpinEdit;
    CbxOuter: TRzComboBox;
    EdtBarColor: TRzColorEdit;
    EdtBackColor: TRzColorEdit;
    GrpTRzLabel: TRzGroupBox;
    LblPreview: TRzLabel;
    GrpTextStyle: TRzRadioGroup;
    GrpRotation: TRzGroupBox;
    LblAngle: TRzLabel;
    TrkAngle: TRzTrackBar;
    RzAnimator1: TRzAnimator;
    GrpTRzLEDDisplay: TRzGroupBox;
    LedPreview: TRzLEDDisplay;
    RzLabel1: TRzLabel;
    EdtLedCaption: TRzEdit;
    GrpDirection: TRzRadioGroup;
    ChkScroll: TRzCheckBox;
    RzGroupBox1: TRzGroupBox;
    RzVersionInfoStatus2: TRzVersionInfoStatus;
    RzVersionInfoStatus3: TRzVersionInfoStatus;
    RzVersionInfoStatus1: TRzVersionInfoStatus;
    RzVersionInfoStatus4: TRzVersionInfoStatus;
    ImageList1: TImageList;
    RzVersionInfo1: TRzVersionInfo;
    TrkBorderWidth: TRzTrackBar;
    RzLabel3: TRzLabel;
    RzLabel4: TRzLabel;
    RzLabel5: TRzLabel;
    pnlHeader: TRzPanel;
    edtBarColorStop: TRzColorEdit;
    edtBackColorStop: TRzColorEdit;
    grpBarStyle: TRzRadioGroup;
    grpBorder: TRzGroupBox;
    RzGroupBox2: TRzGroupBox;
    RzLabel6: TRzLabel;
    RzLabel7: TRzLabel;
    RzLabel8: TRzLabel;
    RzLabel9: TRzLabel;
    procedure GrpTextStyleClick(Sender: TObject);
    procedure TrkAngleDrawTick( TrackBar: TRzTrackBar; Canvas: TCanvas; Location: TPoint; Index: Integer );
    procedure TrkAngleChange(Sender: TObject);
    procedure TrkPercentChange(Sender: TObject);
    procedure SpnPartsCompChange(Sender: TObject);
    procedure SpnTotalChange(Sender: TObject);
    procedure SpnPartsCompChanging( Sender: TObject; NewValue: Extended;
                                    var AllowChange: Boolean );
    procedure EdtLedCaptionChange(Sender: TObject);
    procedure CbxOuterChange(Sender: TObject);
    procedure ChkScrollClick(Sender: TObject);
    procedure GrpDirectionClick(Sender: TObject);
    procedure EdtBarColorChange(Sender: TObject);
    procedure EdtBackColorChange(Sender: TObject);
    procedure TrkBorderWidthChange(Sender: TObject);
    procedure grpBarStyleClick(Sender: TObject);
    procedure edtBarColorStopChange(Sender: TObject);
    procedure edtBackColorStopChange(Sender: TObject);
  public
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}


uses
  SysUtils;


procedure TFmeDisplay.Init;
begin
  ParentBackground := False;
end;


procedure TFmeDisplay.UpdateVisualStyle( VS: TRzVisualStyle;
                                         GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
end;


procedure TFmeDisplay.GrpTextStyleClick(Sender: TObject);
begin
  LblPreview.TextStyle := TTextStyle( GrpTextStyle.ItemIndex );
end;


procedure TFmeDisplay.TrkAngleDrawTick(TrackBar: TRzTrackBar;
  Canvas: TCanvas; Location: TPoint; Index: Integer);
var
  W, Degree : Integer;
begin
  Degree := Index;
  if ( Degree mod 90 ) = 0 then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := 'Small Fonts';
    Canvas.Font.Size := 7;
    Canvas.Font.Style := [];
    W := Canvas.TextWidth( IntToStr( Degree ) );
    Canvas.TextOut( Location.X - (W div 2), 1, IntToStr( Degree ) );
  end;
end;


procedure TFmeDisplay.TrkAngleChange(Sender: TObject);
var
  Angle : Integer;
begin
  Angle := TrkAngle.Position;
  LblAngle.Caption := IntToStr( Angle ) + '°';
  LblPreview.Angle := Angle;
end;



procedure TFmeDisplay.TrkPercentChange(Sender: TObject);
begin
  SpnPartsComp.Value := 0;
  SpnTotal.Value := 0;
  PbrPreview.Percent := TrkPercent.Position;
end;

procedure TFmeDisplay.SpnPartsCompChange(Sender: TObject);
begin
  PbrPreview.PartsComplete := SpnPartsComp.IntValue;
end;

procedure TFmeDisplay.SpnTotalChange(Sender: TObject);
begin
  SpnPartsComp.Value := 0;
  SpnPartsCompChange( Self );

  PbrPreview.TotalParts := SpnTotal.IntValue;
end;

procedure TFmeDisplay.SpnPartsCompChanging( Sender: TObject; NewValue: Extended;
                                            var AllowChange: Boolean );
begin
  if SpnTotal.IntValue = 0 then
    AllowChange := False;
end;

procedure TFmeDisplay.EdtLedCaptionChange(Sender: TObject);
begin
  LedPreview.Caption := EdtLedCaption.Text;
end;

procedure TFmeDisplay.CbxOuterChange(Sender: TObject);
begin
  PbrPreview.BorderOuter := TFrameStyle( CbxOuter.ItemIndex );
end;

procedure TFmeDisplay.ChkScrollClick(Sender: TObject);
begin
  LedPreview.Scrolling := ChkScroll.Checked;
  GrpDirection.Enabled := ChkScroll.Checked;
end;

procedure TFmeDisplay.GrpDirectionClick(Sender: TObject);
begin
  if GrpDirection.ItemIndex = 0 then
    LedPreview.ScrollType := stRightToLeft
  else
    LedPreview.ScrollType := stLeftToRight;
end;


procedure TFmeDisplay.grpBarStyleClick(Sender: TObject);
begin
  pbrPreview.BarStyle := TBarStyle( grpBarStyle.ItemIndex );
end;


procedure TFmeDisplay.TrkBorderWidthChange(Sender: TObject);
begin
  PbrPreview.BorderWidth := TrkBorderWidth.Position;
end;


procedure TFmeDisplay.EdtBarColorChange(Sender: TObject);
begin
  pbrPreview.BarColor := edtBarColor.SelectedColor;
end;


procedure TFmeDisplay.EdtBackColorChange(Sender: TObject);
begin
  PbrPreview.BackColor := edtBackColor.SelectedColor;
  PbrPreview.BorderColor := edtBackColor.SelectedColor;
end;


procedure TFmeDisplay.edtBarColorStopChange(Sender: TObject);
begin
  pbrPreview.BarColorStop := edtBarColorStop.SelectedColor;
end;


procedure TFmeDisplay.edtBackColorStopChange(Sender: TObject);
begin
  pbrPreview.BackColorStop := edtBackColorStop.SelectedColor;
end;

end.


