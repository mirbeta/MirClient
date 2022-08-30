unit BackgroundDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, dxBar, ImgList, dxBarExtItems, ActnList,
  Buttons, ExtDlgs, cxClasses, EBarsUtils;

type
  TBackgroundDemoMainForm = class(TForm)
    BarManagerStyle: TRadioGroup;
    dxBarManager1: TdxBarManager;
    dxBarSubItem1: TdxBarSubItem;
    dxBarSubItem2: TdxBarSubItem;
    dxBarSubItem3: TdxBarSubItem;
    dxBarSubItem4: TdxBarSubItem;
    dxBarSubItem5: TdxBarSubItem;
    dxBarSubItem6: TdxBarSubItem;
    dxBarButton1: TdxBarLargeButton;
    dxBarButton3: TdxBarLargeButton;
    dxBarButton4: TdxBarLargeButton;
    dxBarButton5: TdxBarLargeButton;
    dxBarButton6: TdxBarLargeButton;
    dxBarButton7: TdxBarLargeButton;
    dxBarButton8: TdxBarLargeButton;
    dxBarButton9: TdxBarLargeButton;
    dxBarButton10: TdxBarLargeButton;
    dxBarButton11: TdxBarLargeButton;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarLargeButton2: TdxBarLargeButton;
    dxBarLargeButton3: TdxBarLargeButton;
    dxBarLargeButton4: TdxBarLargeButton;
    dxBarLargeButton5: TdxBarLargeButton;
    dxBarLargeButton6: TdxBarLargeButton;
    dxBarLargeButton7: TdxBarLargeButton;
    dxBarButton15: TdxBarButton;
    dxBarColorCombo1: TdxBarColorCombo;
    dxBarFontNameCombo1: TdxBarFontNameCombo;
    dxBarDateCombo1: TdxBarDateCombo;
    dxBarSpinEdit1: TdxBarSpinEdit;
    dxBarProgressItem1: TdxBarProgressItem;
    dxBarStatic1: TdxBarStatic;
    dxBarPopupMenu1: TdxBarPopupMenu;
    dxBarLargeButton8: TdxBarLargeButton;
    dxBarLargeButton9: TdxBarLargeButton;
    dxBarLargeButton10: TdxBarLargeButton;
    dxBarLargeButton11: TdxBarLargeButton;
    dxBarLargeButton12: TdxBarLargeButton;
    dxBarLargeButton13: TdxBarLargeButton;
    dxBarLargeButton14: TdxBarLargeButton;
    gbBackgroundOptions: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbBar: TCheckBox;
    cbSubMenu: TCheckBox;
    cbBackgroundBitmap: TCheckBox;
    cbBackgroundBitmapPM: TCheckBox;
    gbImageDisplay: TGroupBox;
    btnNewWindow: TdxBarLargeButton;
    btnArrangeAll: TdxBarLargeButton;
    btnSplit: TdxBarLargeButton;
    btnNew: TdxBarLargeButton;
    btnOpen: TdxBarLargeButton;
    btnSave: TdxBarLargeButton;
    btnClose: TdxBarLargeButton;
    btnPrint: TdxBarLargeButton;
    btnSaveAs: TdxBarLargeButton;
    btnSaveAll: TdxBarLargeButton;
    dxBarSubItem7: TdxBarSubItem;
    sbBarBackgroud: TSpeedButton;
    sbBarSubmenu: TSpeedButton;
    sbSatusbarBackgroud: TSpeedButton;
    sbPopupmenu: TSpeedButton;
    lbBarBackgroud: TLabel;
    lbBarSubmenu: TLabel;
    lbSatusbarBackgroud: TLabel;
    lbPopupmenu: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    btnRestoreDefaults: TButton;
    Bevel1: TBevel;
    cbShowImageCaptions: TCheckBox;
    cbAssignHotImages: TCheckBox;
    Label4: TLabel;
    Panel1: TPanel;
    dxBarLargeButton15: TdxBarLargeButton;
    dxBarLargeButton16: TdxBarLargeButton;
    ilLargeImages: TImageList;
    ilDisabledImages: TImageList;
    ilHotImages: TImageList;
    pnlOptions: TPanel;
    pnlMain: TPanel;
    procedure BarManagerStyleClick(Sender: TObject);
    procedure dxBarButton1Click(Sender: TObject);
    procedure dxBarSpinEdit1CurChange(Sender: TObject);
    procedure cbBarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbSubMenuClick(Sender: TObject);
    procedure cbBackgroundBitmapClick(Sender: TObject);
    procedure cbBackgroundBitmapPMClick(Sender: TObject);
    procedure sbBarBackgroudClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbShowImageCaptionsClick(Sender: TObject);
    procedure cbAssignHotImagesClick(Sender: TObject);
  private
    FBarBackgroudBitmap, FBarSubmenuBitmap, FSatusbarBackgroudBitmap, FPopupmenuBitmap: TBitmap;
  public
    FPath: string;
  end;

var
  BackgroundDemoMainForm: TBackgroundDemoMainForm;

implementation

const
  sCustomImage = '(custom image)';
  sDefaultImage = '(default image)';

{$R *.dfm}

procedure TBackgroundDemoMainForm.BarManagerStyleClick(Sender: TObject);
begin
  dxBarManager1.Style := TdxBarManagerStyle(BarManagerStyle.ItemIndex);
end;

procedure TBackgroundDemoMainForm.dxBarButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TBackgroundDemoMainForm.dxBarSpinEdit1CurChange(Sender: TObject);
begin
  dxBarProgressItem1.Position := Trunc(dxBarSpinEdit1.curValue);
  dxBarSpinEdit1.curValue := dxBarProgressItem1.Position;
end;

procedure TBackgroundDemoMainForm.FormCreate(Sender: TObject);
begin
  FPath := ExtractFilePath(Application.ExeName);

  FBarBackgroudBitmap := TBitmap.Create;
  FBarSubmenuBitmap := TBitmap.Create;
  FSatusbarBackgroudBitmap := TBitmap.Create;
  FPopupmenuBitmap := TBitmap.Create;

  cbBarClick(nil);
  cbSubMenuClick(nil);
  cbBackgroundBitmapClick(nil);
  cbBackgroundBitmapPMClick(nil);
end;

procedure TBackgroundDemoMainForm.FormDestroy(Sender: TObject);
begin
  FBarBackgroudBitmap.Free;
  FBarSubmenuBitmap.Free;
  FSatusbarBackgroudBitmap.Free;
  FPopupmenuBitmap.Free;
end;

procedure TBackgroundDemoMainForm.cbBarClick(Sender: TObject);
begin
  if cbBar.Checked then
  begin
    if lbBarBackgroud.Caption = sDefaultImage then
      dxBarManager1.Backgrounds.Bar.LoadFromFile(FPath + 'b01.bmp')
    else
      dxBarManager1.Backgrounds.Bar.Assign(FBarBackgroudBitmap);
    lbBarBackgroud.Enabled := True;
    sbBarBackgroud.Enabled := True;
  end
  else
  begin
    dxBarManager1.Backgrounds.Bar.Assign(nil);
    lbBarBackgroud.Enabled := False;
    sbBarBackgroud.Enabled := False;
  end;
end;

procedure TBackgroundDemoMainForm.cbSubMenuClick(Sender: TObject);
begin
  if cbSubMenu.Checked then
  begin
    if lbBarSubmenu.Caption = sDefaultImage then
      dxBarManager1.Backgrounds.SubMenu.LoadFromFile(FPath + 'b02.bmp')
    else
      dxBarManager1.Backgrounds.SubMenu.Assign(FBarSubmenuBitmap);
    lbBarSubmenu.Enabled := True;
    sbBarSubmenu.Enabled := True;
  end
  else
  begin
    dxBarManager1.Backgrounds.SubMenu.Assign(nil);
    lbBarSubmenu.Enabled := False;
    sbBarSubmenu.Enabled := False;
  end
end;

procedure TBackgroundDemoMainForm.cbBackgroundBitmapClick(Sender: TObject);
begin
  if cbBackgroundBitmap.Checked then
  begin
    if lbSatusbarBackgroud.Caption = sDefaultImage then
      dxBarManager1.Bars[4].BackgroundBitmap.LoadFromFile(FPath + 'b03.bmp')
    else
      dxBarManager1.Bars[4].BackgroundBitmap.Assign(FSatusbarBackgroudBitmap);
    lbSatusbarBackgroud.Enabled := True;
    sbSatusbarBackgroud.Enabled := True;
  end
  else
  begin
    dxBarManager1.Bars[4].BackgroundBitmap.Assign(nil);
    lbSatusbarBackgroud.Enabled := False;
    sbSatusbarBackgroud.Enabled := False;
  end
end;

procedure TBackgroundDemoMainForm.cbBackgroundBitmapPMClick(Sender: TObject);
begin
  if cbBackgroundBitmapPM.Checked then
  begin
    if lbPopupmenu.Caption = sDefaultImage then
      dxBarPopupMenu1.BackgroundBitmap.LoadFromFile(FPath + 'b04.bmp')
    else
      dxBarPopupMenu1.BackgroundBitmap.Assign(FPopupmenuBitmap);
    lbPopupmenu.Enabled := True;
    sbPopupmenu.Enabled := True;
  end
  else
  begin
    lbPopupmenu.Enabled := False;
    sbPopupmenu.Enabled := False;
    dxBarPopupMenu1.BackgroundBitmap.Assign(nil);
  end;
end;

procedure TBackgroundDemoMainForm.sbBarBackgroudClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    case TComponent(Sender).Tag of
      0: begin
          FBarBackgroudBitmap.LoadFromFile(OpenPictureDialog.FileName);
          dxBarManager1.Backgrounds.Bar.Assign(FBarBackgroudBitmap);
          lbBarBackgroud.Caption := sCustomImage;
         end;
      1: begin
          FBarSubmenuBitmap.LoadFromFile(OpenPictureDialog.FileName);
          dxBarManager1.Backgrounds.SubMenu.Assign(FBarSubmenuBitmap);
          lbBarSubmenu.Caption := sCustomImage;
         end;
      2: begin
           FSatusbarBackgroudBitmap.LoadFromFile(OpenPictureDialog.FileName);
           dxBarManager1.Bars[4].BackgroundBitmap.Assign(FSatusbarBackgroudBitmap);
           lbSatusbarBackgroud.Caption := sCustomImage;
         end;
      3: begin
          FPopupmenuBitmap.LoadFromFile(OpenPictureDialog.FileName);
          dxBarPopupMenu1.BackgroundBitmap.Assign(FPopupmenuBitmap);
          lbPopupmenu.Caption := sCustomImage;
         end;
    end;
end;

procedure TBackgroundDemoMainForm.btnRestoreDefaultsClick(Sender: TObject);
begin
  if (not cbBar.Checked) or (lbBarBackgroud.Caption = sCustomImage) then
  begin
    dxBarManager1.Backgrounds.Bar.LoadFromFile(FPath + 'b01.bmp');
    lbBarBackgroud.Caption := sDefaultImage;
    cbBar.Checked := True;
  end;

  if (not cbSubMenu.Checked) or (lbBarSubmenu.Caption = sCustomImage) then
  begin
    dxBarManager1.Backgrounds.SubMenu.LoadFromFile(FPath + 'b02.bmp');
    lbBarSubmenu.Caption := sDefaultImage;
    cbSubMenu.Checked := True;
  end;

  if (not cbBackgroundBitmap.Checked) or (lbSatusbarBackgroud.Caption = sCustomImage) then
  begin
    dxBarManager1.Bars[4].BackgroundBitmap.LoadFromFile(FPath + 'b03.bmp');
    lbSatusbarBackgroud.Caption := sDefaultImage;
    cbBackgroundBitmap.Checked := True;
  end;

  if (not cbBackgroundBitmapPM.Checked) or (lbPopupmenu.Caption = sCustomImage) then
  begin
    dxBarPopupMenu1.BackgroundBitmap.LoadFromFile(FPath + 'b04.bmp');
    lbPopupmenu.Caption := sDefaultImage;
    cbBackgroundBitmapPM.Checked := True;
  end;
end;

procedure TBackgroundDemoMainForm.cbShowImageCaptionsClick(
  Sender: TObject);
var
  i: Integer;
  AChecked: Boolean;
begin
  dxBarManager1.BeginUpdate;
  AChecked := TCheckBox(Sender).Checked;
  for i:=0 to ComponentCount - 1 do
    if Components[i] is TdxBarLargeButton then
      TdxBarLargeButton(Components[i]).ShowCaption := AChecked;
  dxBarManager1.EndUpdate;
end;

procedure TBackgroundDemoMainForm.cbAssignHotImagesClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    dxBarManager1.HotImages := ilHotImages
  else
    dxBarManager1.HotImages := nil;
end;

end.
