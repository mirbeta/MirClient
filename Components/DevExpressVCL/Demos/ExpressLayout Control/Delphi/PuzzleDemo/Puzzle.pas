unit Puzzle;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Types, Dialogs, StdCtrls, ExtCtrls, dxLayoutControl, cxControls,
  dxLayoutLookAndFeels, Menus, ActnList, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, cxClasses;

type
  TfrmPuzzle = class(TForm)
    lcMain: TdxLayoutControl;
    lcMainGroup_Root1: TdxLayoutGroup;
    lcItem1: TdxLayoutImageItem;
    dxLayoutLookAndFeelList10: TdxLayoutLookAndFeelList;
    dxLayoutStandardLookAndFeel10: TdxLayoutStandardLookAndFeel;
    lcItem2: TdxLayoutImageItem;
    lcItem3: TdxLayoutImageItem;
    lcItem4: TdxLayoutImageItem;
    lcItem5: TdxLayoutImageItem;
    lcItem6: TdxLayoutImageItem;
    lcItem7: TdxLayoutImageItem;
    lcItem8: TdxLayoutImageItem;
    lcItem9: TdxLayoutImageItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup5: TdxLayoutGroup;
    lcMainGroup6: TdxLayoutGroup;
    MainMenu1: TMainMenu;
    About1: TMenuItem;
    procedure Shufflepuzzle1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  end;

var
  frmPuzzle: TfrmPuzzle;

implementation

{$R *.dfm}

type
  TdxLayoutContainerAccess = class(TdxLayoutContainer);

procedure TfrmPuzzle.Shufflepuzzle1Click(Sender: TObject);
var
  AList: TList;
  I, J: Integer;
  AItem: TdxLayoutItem;
  AGroup: TdxCustomLayoutGroup;
begin
  AList := TList.Create;
  try
    AList.Add(lcItem1);
    AList.Add(lcItem2);
    AList.Add(lcItem3);
    AList.Add(lcItem4);
    AList.Add(lcItem5);
    AList.Add(lcItem6);
    AList.Add(lcItem7);
    AList.Add(lcItem8);
    AList.Add(lcItem9);
    lcMain.BeginUpdate;
    try
      lcMain.Items.LayoutDirection := ldVertical;
      Randomize;
      for I := 0 to 2 do
      begin
        AGroup := lcMain.CreateGroup(TdxLayoutGroup, lcMainGroup_Root1);
        AGroup.LayoutDirection := ldHorizontal;
        AGroup.Hidden := True;
        AGroup.Move(lcMain.Items, 0);
        for J := 0 to 2 do
        begin
          AItem := TdxLayoutItem(AList[Random(AList.Count)]);
          AList.Extract(AItem);
          AItem.MoveTo(AGroup, J);
        end;
      end;
    finally
      lcMain.EndUpdate;
    end;
  finally
    AList.Free;
  end;
end;

procedure TfrmPuzzle.FormCreate(Sender: TObject);
begin
  Position := poDesigned;
  TdxLayoutContainerAccess(lcMain.Container).ShowQuickCustomizationToolbar := False;
end;

procedure TfrmPuzzle.FormShow(Sender: TObject);
begin
  Shufflepuzzle1Click(nil);
end;

procedure TfrmPuzzle.About1Click(Sender: TObject);
begin
  ShowMessage('Solve the puzzle by dragging items');
end;

end.
