unit DesktopDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxClasses, dxSkinsForm, dxCustomTileControl, dxTileControl, dxSkinsCore,
  dxSkinsDefaultPainters, jpeg, dxGDIPlusClasses, ImgList;

type
  TDesktopDemoMainForm = class(TForm)
    dxTileControl1: TdxTileControl;
    ItemDesktop: TdxTileControlItem;
    dxTileControl1Group1: TdxTileControlGroup;
    ItemPeople: TdxTileControlItem;
    ItemMail: TdxTileControlItem;
    ItemGames: TdxTileControlItem;
    ItemCamera: TdxTileControlItem;
    ItemVideo: TdxTileControlItem;
    ItemMusic: TdxTileControlItem;
    ItemSkyDrive: TdxTileControlItem;
    ItemStore: TdxTileControlItem;
    dxTileControl1Group2: TdxTileControlGroup;
    ItemCalendar: TdxTileControlItem;
    ItemMaps: TdxTileControlItem;
    ItemReadingList: TdxTileControlItem;
    ItemSports: TdxTileControlItem;
    ItemNews: TdxTileControlItem;
    ItemIE: TdxTileControlItem;
    ItemPhotos: TdxTileControlItem;
    ItemFinances: TdxTileControlItem;
    ItemWeather: TdxTileControlItem;
    ItemFoods: TdxTileControlItem;
    dxTileControl1Group3: TdxTileControlGroup;
    ItemTravel: TdxTileControlItem;
    ItemHealth: TdxTileControlItem;
    ItemHelp: TdxTileControlItem;
    icPeople: TcxImageCollection;
    icPeopleItem1: TcxImageCollectionItem;
    icPeopleItem2: TcxImageCollectionItem;
    icPeopleItem3: TcxImageCollectionItem;
    icPeopleItem4: TcxImageCollectionItem;
    icPeopleItem5: TcxImageCollectionItem;
    icPeopleItem6: TcxImageCollectionItem;
    icPeopleItem7: TcxImageCollectionItem;
    icPeopleItem8: TcxImageCollectionItem;
    icPeopleItem9: TcxImageCollectionItem;
    icPeopleItem10: TcxImageCollectionItem;
    icPeopleItem11: TcxImageCollectionItem;
    icPeopleItem12: TcxImageCollectionItem;
    ItemFoodsdxTileControlItemFrame1: TdxTileControlItemFrame;
    ItemHealthdxTileControlItemFrame1: TdxTileControlItemFrame;
    ItemFoodsdxTileControlItemFrame2: TdxTileControlItemFrame;
    ItemHealthdxTileControlItemFrame2: TdxTileControlItemFrame;
    tcaCustomizeOn: TdxTileControlActionBarItem;
    tcaExit: TdxTileControlActionBarItem;
    ilMedium: TcxImageList;
    ilSmall: TcxImageList;
    tcaClearSelection: TdxTileControlActionBarItem;
    procedure FormShow(Sender: TObject);
    procedure ItemFoodsActiveFrameChanged(Sender: TdxTileControlItem);
    procedure tcaExitClick(Sender: TdxTileControlActionBarItem);
    procedure tcaCustomizeOnClick(Sender: TdxTileControlActionBarItem);
    procedure dxTileControl1KeyPress(Sender: TObject; var Key: Char);
    procedure tcaClearSelectionClick(Sender: TdxTileControlActionBarItem);
    procedure dxTileControl1ItemCheck(Sender: TdxCustomTileControl; AItem: TdxTileControlItem);
  private
    procedure UpdateActionBarsItems;
  end;

var
  DesktopDemoMainForm: TDesktopDemoMainForm;

implementation

uses
  DateUtils, dxAnimation;

{$R *.dfm}

function GetDayName(ADayOfWeek: Word): string;
begin
  case ADayOfWeek of
    1: Result := 'Sunday';
    2: Result := 'Monday';
    3: Result := 'Tuesday';
    4: Result := 'Wednesday';
    5: Result := 'Thursday';
    6: Result := 'Friday';
  else
    Result := 'Saturday';
  end;
end;

procedure TDesktopDemoMainForm.FormShow(Sender: TObject);
begin
  ItemCalendar.Text2.Value := IntToStr(DayOfTheMonth(Now));
  ItemCalendar.Text4.Value := GetDayName(DayOfWeek(Now));
  UpdateActionBarsItems;
end;

procedure TDesktopDemoMainForm.ItemFoodsActiveFrameChanged(Sender: TdxTileControlItem);
const
  AAnimationModes: array[Boolean] of TdxDrawAnimationMode = (amScrollUp, amScrollDown);
begin
  Sender.AnimationMode := AAnimationModes[Sender.AnimationMode = amScrollUp];
end;

procedure TDesktopDemoMainForm.tcaCustomizeOnClick(Sender: TdxTileControlActionBarItem);
begin
  dxTileControl1.OptionsBehavior.GroupRenaming := True;
end;

procedure TDesktopDemoMainForm.dxTileControl1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key <> #27 then Exit;
  if dxTileControl1.DragAndDropState <> ddsNone then
    dxTileControl1.FinishDragAndDrop(False)
  else
    if dxTileControl1.OptionsBehavior.GroupRenaming then
      dxTileControl1.OptionsBehavior.GroupRenaming := False
    else
      Close;
end;

procedure TDesktopDemoMainForm.tcaExitClick(Sender: TdxTileControlActionBarItem);
begin
  Close;
end;

procedure TDesktopDemoMainForm.tcaClearSelectionClick(Sender: TdxTileControlActionBarItem);
var
  I: Integer;
begin
  for I := dxTileControl1.CheckedItemCount - 1 downto 0 do
    dxTileControl1.CheckedItems[I].Checked := False;
end;

procedure TDesktopDemoMainForm.UpdateActionBarsItems;
begin
  tcaClearSelection.Visible := dxTileControl1.CheckedItemCount > 0;
end;

procedure TDesktopDemoMainForm.dxTileControl1ItemCheck(Sender: TdxCustomTileControl; AItem: TdxTileControlItem);
begin
  UpdateActionBarsItems;
end;

end.
