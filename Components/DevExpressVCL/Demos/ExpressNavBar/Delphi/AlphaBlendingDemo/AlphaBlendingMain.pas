unit AlphaBlendingMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ImgList, ExtDlgs,
  dxNavBarStyles, dxNavBarCollns, dxNavBarBase, dxNavBar;

type
  TfmAlphaBlendingMain = class(TForm)
    nbMain: TdxNavBar;
    Label9: TLabel;
    gbDocs: TdxNavBarGroup;
    bgPlaces: TdxNavBarGroup;
    bgDetails: TdxNavBarGroup;
    biMyMusic: TdxNavBarItem;
    biMyPictures: TdxNavBarItem;
    biReceivedFiles: TdxNavBarItem;
    biDesktop: TdxNavBarItem;
    biMyComputer: TdxNavBarItem;
    biNetwork: TdxNavBarItem;
    ilSmall: TImageList;
    ilLarge: TImageList;
    stBackground: TdxNavBarStyleItem;
    stGroup1Background: TdxNavBarStyleItem;
    stGroup2Background: TdxNavBarStyleItem;
    stGroup3Background: TdxNavBarStyleItem;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel5: TPanel;
    GroupBox4: TGroupBox;
    Label7: TLabel;
    lbBgGroup3: TLabel;
    tbBgGroup3: TTrackBar;
    Panel4: TPanel;
    iBgGroup3: TImage;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    lbBgGroup2: TLabel;
    tbBgGroup2: TTrackBar;
    Panel3: TPanel;
    iBgGroup2: TImage;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    lbBgGroup1: TLabel;
    tbBgGroup1: TTrackBar;
    Panel2: TPanel;
    iBgGroup1: TImage;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lbBg: TLabel;
    Panel1: TPanel;
    iBg: TImage;
    tbBg: TTrackBar;
    procedure lbBgDblClick(Sender: TObject);
    procedure lbBgGroup1DblClick(Sender: TObject);
    procedure lbBgGroup2DblClick(Sender: TObject);
    procedure lbBgGroup3DblClick(Sender: TObject);
    procedure iBgClick(Sender: TObject);
    procedure iBgGroup1Click(Sender: TObject);
    procedure iBgGroup2Click(Sender: TObject);
    procedure iBgGroup3Click(Sender: TObject);
    procedure tbBgChange(Sender: TObject);
    procedure tbBgGroup1Change(Sender: TObject);
    procedure tbBgGroup2Change(Sender: TObject);
    procedure tbBgGroup3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure nbMainGroupHotTrack(Sender: TObject;
      Group: TdxNavBarGroup);
    procedure nbMainCalcGroupClientHeight(Sender: TObject;
      ViewInfo: TdxNavBarGroupViewInfo; var Height: Integer);
    procedure nbMainCustomDrawGroupClientForeground(Sender: TObject;
      Canvas: TCanvas; ViewInfo: TdxNavBarGroupViewInfo;
      var Handled: Boolean);
    procedure nbMainGetGroupHint(Sender: TObject; Group: TdxNavBarGroup;
      var Hint: String);
    procedure nbMainCalcGroupHintRect(Sender: TObject;
      Group: TdxNavBarGroup; ViewInfo: TdxNavBarViewInfo; var R: TRect);
    procedure nbMainCustomDrawGroupHint(Sender: TObject;
      ACanvas: TCanvas; AGroup: TdxNavBarGroup; AHint: String; R: TRect;
      var AHandled: Boolean);
  private
    procedure ClearBgImage(AStyleItem: TdxNavBarStyleItem);
    procedure SetBgImage(AStyleItem: TdxNavBarStyleItem);
    procedure SetBgAlphaBlending(AStyleItem: TdxNavBarStyleItem; AValue: Byte);
  public
    { Public declarations }
  end;

var
  fmAlphaBlendingMain: TfmAlphaBlendingMain;

implementation

{$R *.dfm}

{ TForm1 }

procedure TfmAlphaBlendingMain.ClearBgImage(AStyleItem: TdxNavBarStyleItem);
begin
  AStyleItem.Style.Image.Graphic := nil;
end;

procedure TfmAlphaBlendingMain.SetBgAlphaBlending(AStyleItem: TdxNavBarStyleItem; AValue: Byte);
begin
  AStyleItem.Style.AlphaBlending := AValue;
  AStyleItem.Style.AlphaBlending2 := AValue;
end;

procedure TfmAlphaBlendingMain.SetBgImage(AStyleItem: TdxNavBarStyleItem);
begin
  if OpenPictureDialog1.Execute then
    AStyleItem.Style.Image.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TfmAlphaBlendingMain.lbBgDblClick(Sender: TObject);
begin
  ClearBgImage(nbMain.StyleBackground);
  iBg.Picture.Graphic := nil;
end;

procedure TfmAlphaBlendingMain.lbBgGroup1DblClick(Sender: TObject);
begin
  ClearBgImage(nbMain.Groups[0].StyleBackground);
  iBgGroup1.Picture.Graphic := nil;
end;

procedure TfmAlphaBlendingMain.lbBgGroup2DblClick(Sender: TObject);
begin
  ClearBgImage(nbMain.Groups[1].StyleBackground);
  iBgGroup2.Picture.Graphic := nil;
end;

procedure TfmAlphaBlendingMain.lbBgGroup3DblClick(Sender: TObject);
begin
  ClearBgImage(nbMain.Groups[2].StyleBackground);
  iBgGroup3.Picture.Graphic := nil;
end;

procedure TfmAlphaBlendingMain.iBgClick(Sender: TObject);
begin
  SetBgImage(nbMain.StyleBackground);
  iBg.Picture.Assign(nbMain.StyleBackground.Style.Image);
end;

procedure TfmAlphaBlendingMain.iBgGroup1Click(Sender: TObject);
begin
  SetBgImage(nbMain.Groups[0].StyleBackground);
  iBgGroup1.Picture.Assign(nbMain.Groups[0].StyleBackground.Style.Image);
end;

procedure TfmAlphaBlendingMain.iBgGroup2Click(Sender: TObject);
begin
  SetBgImage(nbMain.Groups[1].StyleBackground);
  iBgGroup2.Picture.Assign(nbMain.Groups[1].StyleBackground.Style.Image);
end;

procedure TfmAlphaBlendingMain.iBgGroup3Click(Sender: TObject);
begin
  SetBgImage(nbMain.Groups[2].StyleBackground);
  iBgGroup3.Picture.Assign(nbMain.Groups[2].StyleBackground.Style.Image);
end;

procedure TfmAlphaBlendingMain.tbBgChange(Sender: TObject);
begin
  SetBgAlphaBlending(nbMain.StyleBackground, tbBg.Position);
end;

procedure TfmAlphaBlendingMain.tbBgGroup1Change(Sender: TObject);
begin
  SetBgAlphaBlending(nbMain.Groups[0].StyleBackground, tbBgGroup1.Position);
end;

procedure TfmAlphaBlendingMain.tbBgGroup2Change(Sender: TObject);
begin
  SetBgAlphaBlending(nbMain.Groups[1].StyleBackground, tbBgGroup2.Position);
end;

procedure TfmAlphaBlendingMain.tbBgGroup3Change(Sender: TObject);
begin
  SetBgAlphaBlending(nbMain.Groups[2].StyleBackground, tbBgGroup3.Position);
end;

procedure TfmAlphaBlendingMain.FormCreate(Sender: TObject);
begin
  tbBg.Position := nbMain.StyleBackground.Style.AlphaBlending;
  tbBgGroup1.Position := nbMain.Groups[0].StyleBackground.Style.AlphaBlending;
  tbBgGroup2.Position := nbMain.Groups[1].StyleBackground.Style.AlphaBlending;
  tbBgGroup3.Position := nbMain.Groups[2].StyleBackground.Style.AlphaBlending;

  iBg.Picture.Assign(nbMain.StyleBackground.Style.Image);
  iBgGroup1.Picture.Assign(nbMain.Groups[0].StyleBackground.Style.Image);
  iBgGroup2.Picture.Assign(nbMain.Groups[1].StyleBackground.Style.Image);
  iBgGroup3.Picture.Assign(nbMain.Groups[2].StyleBackground.Style.Image);
end;

procedure TfmAlphaBlendingMain.nbMainGroupHotTrack(Sender: TObject;
  Group: TdxNavBarGroup);
begin
  if Group = bgDetails then
    bgDetails.LargeImageIndex := 2
  else bgDetails.LargeImageIndex := 3;
end;

procedure TfmAlphaBlendingMain.nbMainCalcGroupClientHeight(Sender: TObject;
  ViewInfo: TdxNavBarGroupViewInfo; var Height: Integer);
begin
  if (ViewInfo.Group = bgDetails) and ViewInfo.Group.Expanded then
    Height := 50;
end;

procedure TfmAlphaBlendingMain.nbMainCustomDrawGroupClientForeground(Sender: TObject;
  Canvas: TCanvas; ViewInfo: TdxNavBarGroupViewInfo; var Handled: Boolean);
begin
  if (ViewInfo.Group = bgDetails) and ViewInfo.Group.Expanded then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clBlack;
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(ViewInfo.ItemsRect.Left + 10, ViewInfo.ItemsRect.Top + 10, 'My Documents');
    Canvas.Font.Style := [];
    Canvas.TextOut(ViewInfo.ItemsRect.Left + 10, ViewInfo.ItemsRect.Top + 24, 'System Folder');
    Handled := True;
  end;
end;

procedure TfmAlphaBlendingMain.nbMainGetGroupHint(Sender: TObject;
  Group: TdxNavBarGroup; var Hint: String);
begin
  if Group = bgDetails then
    Hint := 'Custom hint for ' + Group.Caption;
end;

procedure TfmAlphaBlendingMain.nbMainCalcGroupHintRect(Sender: TObject;
  Group: TdxNavBarGroup; ViewInfo: TdxNavBarViewInfo; var R: TRect);
begin
  if Group = bgDetails then
  begin
    R.Right := R.Left + 300;
    R.Bottom := R.Top + 30;
  end;
end;

procedure TfmAlphaBlendingMain.nbMainCustomDrawGroupHint(Sender: TObject;
  ACanvas: TCanvas; AGroup: TdxNavBarGroup; AHint: String; R: TRect;
  var AHandled: Boolean);
begin
  if AGroup = bgDetails then
  begin
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clYellow;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := clTeal;
    ACanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := clYellow;
    ACanvas.Font.Size := 14;
    ACanvas.Font.Style := [fsBold];
    DrawText(ACanvas.Handle, PChar(AHint), Length(AHint), R, DT_VCENTER or
      DT_CENTER or DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX);
    AHandled := True;
  end;
end;

end.
