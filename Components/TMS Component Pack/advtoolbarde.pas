{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2005 - 2012                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvToolBarDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Graphics, Comctrls, Windows, Forms, TypInfo, Dialogs, ExtCtrls,
  Controls, AdvGlowButton, ExtDlgs, GDIPicture, AdvToolBar, AdvShapeButton
{$IFDEF TMSPACK}
  , AdvOfficeSelectors, AdvOfficeComboBox
{$ENDIF}
  , DesignIntf, DesignEditors, ContNrs
  ;

type
  TAdvDockPanelEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvToolBarEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvToolBarButtonEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvToolBarContainerEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvGlowButtonEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvToolBarPagerEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvPageEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvQuickAccessToolBarEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TGDIPATBPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

implementation

uses
  SysUtils
  {$IFNDEF TMS_STD}
  {$IFDEF DELPHI6_LVL}
  , VDBConsts
  {$ELSE}
  , DBConsts
  {$ENDIF}
  {$ENDIF}
  ;

function HTMLToRgb(color: tcolor): tcolor;
var
  r,g,b: integer;
begin
  r := (Color and $0000FF);
  g := (Color and $00FF00);
  b := (Color and $FF0000) shr 16;
  Result := b or g or (r shl 16);
end;

function BrightnessColor(Col: TColor; Brightness: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  r1 := Round( (100 + Brightness)/100 * r1 );
  g1 := Round( (100 + Brightness)/100 * g1 );
  b1 := Round( (100 + Brightness)/100 * b1 );

  Result := RGB(r1,g1,b1);
end;


function BrightnessColor(Col: TColor; BR,BG,BB: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  r1 := Round( (100 + BR)/100 * r1 );
  g1 := Round( (100 + BG)/100 * g1 );
  b1 := Round( (100 + BB)/100 * b1 );

  Result := RGB(r1,g1,b1);
end;


{ AdvToolBarEditor }

procedure TAdvToolBarEditor.ExecuteVerb(Index: integer);
var
  ATBButton : TAdvToolBarButton;
  ATBMenuButton: TAdvToolBarMenuButton;
  ATBSeparator: TAdvToolBarSeparator;
  {$IFNDEF TMS_STD}
  DBATBButton: TDBAdvToolBarButton;
  ADBGlowButton: TDBAdvGlowButton;
  {$ENDIF}
  ATB: TAdvToolBar;
  ATBContainer: TAdvToolBarContainer;
  AGlowButton: TAdvGlowButton;
  AGlowMenuButton: TAdvGlowMenuButton;
{$IFDEF TMSPACK}
  ACOLSelector: TAdvOfficeColorSelector;
  ATCOLSelector: TAdvOfficeTextColorSelector;
  ASHADSelector: TAdvOfficeShadowSelector;
  AGRADSelector: TAdvOfficeGradientDirectionSelector;
  ATABLESelector: TAdvOfficeTableSelector;
  ATBORDERSelector: TAdvOfficeTableBorderSelector;
  APENSSelector: TAdvOfficePenStyleSelector;
  APENWSelector: TAdvOfficePenWidthSelector;
  ABRUSHSelector: TAdvOfficeBrushStyleSelector;
  ACHARSelector: TAdvOfficeCharacterSelector;
  ATOOLSelector: TAdvOfficeToolSelector;
  AFONTSelector: TAdvOfficeFontSelector;
  AFSIZESelector: TAdvOfficeFontSizeSelector;
  AFSCROLLSelector : TAdvOfficeScrollSelector;
{$ENDIF}

  I: Integer;
begin
  inherited;

  ATB := TAdvToolBar(Component);

  case Index of
  0:
    begin
      ATBButton := TAdvToolBarButton(Designer.CreateComponent(TAdvToolBarButton,Component,0, 0,45,23));
      ATBButton.Width := 23;
      ATBButton.Height := 22;
      ATBButton.Parent := ATB;
      ATBButton.Caption := '';

      if Assigned(ATB.Images) then
      begin
        if ATBButton.Index < ATB.Images.Count then
          ATBButton.ImageIndex := ATBButton.Index;
      end;
      ATBButton.AdvToolBar := ATB;
    end;
  1:
    begin
      ATBMenuButton := TAdvToolBarMenuButton(Designer.CreateComponent(TAdvToolBarMenuButton,Component,0, 0,45,23));
      ATBMenuButton.Width := 23;
      ATBMenuButton.Height := 22;
      ATBMenuButton.Parent := ATB;
      ATBMenuButton.Caption := '';//ATBMenuButton.Name;
      ATBMenuButton.AdvToolBar := ATB;
    end;
  2:
    begin
      ATBSeparator := TAdvToolBarSeparator(Designer.CreateComponent(TAdvToolBarSeparator,Component,0, 0,10,23));
      ATBSeparator.Parent := ATB;
      ATBSeparator.AdvToolBar := ATB;
    end;
  3:
    begin
      {$IFNDEF TMS_STD}
      DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
      DBATBButton.Width := 23;
      DBATBButton.Height := 22;
      DBATBButton.Parent := ATB;
      DBATBButton.Caption := '';
      DBATBButton.AdvToolBar := ATB;
      {$ENDIF}
    end;
  4:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
        DBATBButton.Width := 23;
        DBATBButton.Height := 22;
        DBATBButton.Parent := ATB;
        DBATBButton.Caption := '';
        DBATBButton.AdvToolBar := ATB;
        case I of
          1:
          begin
            DBATBButton.DBButtonType := dbtFirst;
            DBATBButton.Hint := SFirstRecord;
          end;
          2:
          begin
            DBATBButton.DBButtonType := dbtPrior;
            DBATBButton.Hint := SPriorRecord;
          end;
          3:
          begin
            DBATBButton.DBButtonType := dbtNext;
            DBATBButton.Hint := SNextRecord;
          end;
          4:
          begin
            DBATBButton.DBButtonType := dbtLast;
            DBATBButton.Hint := SLastRecord;
          end;
          5:
          begin
            DBATBButton.DBButtonType := dbtInsert;
            DBATBButton.Hint := SInsertRecord;
          end;
          6:
          begin
            DBATBButton.ConfirmAction := True;
            DBATBButton.DBButtonType := dbtDelete;
            DBATBButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            DBATBButton.DBButtonType := dbtEdit;
            DBATBButton.Hint := SEditRecord;
          end;
          8:
          begin
            DBATBButton.DBButtonType := dbtPost;
            DBATBButton.Hint := SPostEdit;
          end;
          9:
          begin
            DBATBButton.DBButtonType := dbtCancel;
            DBATBButton.Hint := SCancelEdit;
          end;
          10:
          begin
            DBATBButton.DBButtonType := dbtRefresh;
            DBATBButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}      
    end;
  5:
    begin
      ATBContainer := TAdvToolBarContainer(Designer.CreateComponent(TAdvToolBarContainer,Component,0, 0,65,45));
      ATBContainer.Width := 65;
      ATBContainer.Height := 45;
      ATBContainer.Parent := ATB;
      //ATBContainer.Caption := '';
      ATBContainer.AdvToolBar := ATB;
    end;
  6:
    begin
      AGlowButton := TAdvGlowButton(Designer.CreateComponent(TAdvGlowButton,Component,0, 0,23,22));
      AGlowButton.Width := 23;
      AGlowButton.Height := 22;
      AGlowButton.Parent := ATB;
      AGlowButton.FocusType := ftHot;
      if Assigned(ATB.Images) then
        AGlowButton.Images := TImageList(ATB.Images);
      if Assigned(ATB.DisabledImages) then
        AGlowButton.DisabledImages := TImageList(ATB.DisabledImages);
      AGlowButton.Caption := '';
      if Assigned(ATB.ToolBarStyler) then
        AGlowButton.Appearance.Assign(ATB.ToolBarStyler.GlowButtonAppearance);
    end;
  7:
    begin
      AGlowMenuButton := TAdvGlowMenuButton(Designer.CreateComponent(TAdvGlowMenuButton,Component,0, 0,23,22));
      AGlowMenuButton.Width := 32;
      AGlowMenuButton.Height := 22;
      AGlowMenuButton.Parent := ATB;
      AGlowMenuButton.Caption := '';
      AGlowMenuButton.FocusType := ftHot;
      if Assigned(ATB.Images) then
        AGlowMenuButton.Images := TImageList(ATB.Images);
      if Assigned(ATB.DisabledImages) then
        AGlowMenuButton.DisabledImages := TImageList(ATB.DisabledImages);
      if Assigned(ATB.ToolBarStyler) then
        AGlowMenuButton.Appearance.Assign(ATB.ToolBarStyler.GlowButtonAppearance);
    end;
  8:
    begin
      {$IFNDEF TMS_STD}
      ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
      ADBGlowButton.Width := 23;
      ADBGlowButton.Height := 22;
      ADBGlowButton.Parent := ATB;
      ADBGlowButton.FocusType := ftHot;
      if Assigned(ATB.Images) then
        ADBGlowButton.Images := TImageList(ATB.Images);
      if Assigned(ATB.DisabledImages) then
        ADBGlowButton.DisabledImages := TImageList(ATB.DisabledImages);
      if Assigned(ATB.ToolBarStyler) then
        ADBGlowButton.Appearance.Assign(ATB.ToolBarStyler.GlowButtonAppearance);
      ADBGlowButton.Caption := '';
      {$ENDIF}
    end;
  9:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
        ADBGlowButton.Width := 23;
        ADBGlowButton.Height := 22;
        ADBGlowButton.Parent := ATB;
        ADBGlowButton.FocusType := ftHot;
        ADBGlowButton.Caption := '';

        if Assigned(ATB.ToolBarStyler) then
          ADBGlowButton.Appearance.Assign(ATB.ToolBarStyler.GlowButtonAppearance);

        case I of
          1:
          begin
            ADBGlowButton.DBButtonType := dbFirst;
            ADBGlowButton.Hint := SFirstRecord;
          end;
          2:
          begin
            ADBGlowButton.DBButtonType := dbPrior;
            ADBGlowButton.Hint := SPriorRecord;
          end;
          3:
          begin
            ADBGlowButton.DBButtonType := dbNext;
            ADBGlowButton.Hint := SNextRecord;
          end;
          4:
          begin
            ADBGlowButton.DBButtonType := dbLast;
            ADBGlowButton.Hint := SLastRecord;
          end;
          5:
          begin
            ADBGlowButton.DBButtonType := dbInsert;
            ADBGlowButton.Hint := SInsertRecord;
          end;
          6:
          begin
            ADBGlowButton.ConfirmAction := True;
            ADBGlowButton.DBButtonType := dbDelete;
            ADBGlowButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            ADBGlowButton.DBButtonType := dbEdit;
            ADBGlowButton.Hint := SEditRecord;
          end;
          8:
          begin
            ADBGlowButton.DBButtonType := dbPost;
            ADBGlowButton.Hint := SPostEdit;
          end;
          9:
          begin
            ADBGlowButton.DBButtonType := dbCancel;
            ADBGlowButton.Hint := SCancelEdit;
          end;
          10:
          begin
            ADBGlowButton.DBButtonType := dbRefresh;
            ADBGlowButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}
    end;
  {$IFDEF TMSPACK}
  10:
    begin
      ACOLSelector := TAdvOfficeColorSelector(Designer.CreateComponent(TAdvOfficeColorSelector,Component,0, 0,23,22));
      ACOLSelector.Width := 23;
      ACOLSelector.Height := 22;
      ACOLSelector.Parent := ATB;
      ACOLSelector.Caption := '';
      AColSelector.FocusType := ftHot;
    end;
  11:
    begin
      ATCOLSelector := TAdvOfficeTextColorSelector(Designer.CreateComponent(TAdvOfficeTextColorSelector,Component,0, 0,23,22));
      ATCOLSelector.Width := 23;
      ATCOLSelector.Height := 22;
      ATCOLSelector.Parent := ATB;
      ATCOLSelector.Caption := '';
      ATCOLSelector.FocusType := ftHot;
    end;
  12:
    begin
      APENSSelector := TAdvOfficePenStyleSelector(Designer.CreateComponent(TAdvOfficePenStyleSelector,Component,0, 0,23,22));
      APENSSelector.Width := 23;
      APENSSelector.Height := 22;
      APENSSelector.Parent := ATB;
      APENSSelector.Caption := '';
      APENSSelector.FocusType := ftHot;
    end;
  13:
    begin
      APENWSelector := TAdvOfficePenWidthSelector(Designer.CreateComponent(TAdvOfficePenWidthSelector,Component,0, 0,23,22));
      APENWSelector.Width := 23;
      APENWSelector.Height := 22;
      APENWSelector.Parent := ATB;
      APENWSelector.Caption := '';
      APENWSelector.FocusType := ftHot;
    end;
  14:
    begin
      ABRUSHSelector := TAdvOfficeBrushStyleSelector(Designer.CreateComponent(TAdvOfficeBrushStyleSelector,Component,0, 0,23,22));
      ABRUSHSelector.Width := 23;
      ABRUSHSelector.Height := 22;
      ABRUSHSelector.Parent := ATB;
      ABRUSHSelector.Caption := '';
      ABRUSHSelector.FocusType := ftHot;
    end;
  15:
    begin
      AGRADSelector := TAdvOfficeGradientDirectionSelector(Designer.CreateComponent(TAdvOfficeGradientDirectionSelector,Component,0, 0,23,22));
      AGRADSelector.Width := 23;
      AGRADSelector.Height := 22;
      AGRADSelector.Parent := ATB;
      AGRADSelector.Caption := '';
      AGRADSelector.FocusType := ftHot;
    end;
  16:
    begin
      ASHADSelector := TAdvOfficeShadowSelector(Designer.CreateComponent(TAdvOfficeShadowSelector,Component,0, 0,23,22));
      ASHADSelector.Width := 23;
      ASHADSelector.Height := 22;
      ASHADSelector.Parent := ATB;
      ASHADSelector.Caption := '';
      ASHADSelector.FocusType := ftHot;
    end;
  17:
    begin
      ATABLESelector := TAdvOfficeTableSelector(Designer.CreateComponent(TAdvOfficeTableSelector,Component,0, 0,23,22));
      ATABLESelector.Width := 23;
      ATABLESelector.Height := 22;
      ATABLESelector.Parent := ATB;
      ATABLESelector.Caption := '';
      ATABLESelector.FocusType := ftHot;
    end;
  18:
    begin
      ATBORDERSelector := TAdvOfficeTableBorderSelector(Designer.CreateComponent(TAdvOfficeTableBorderSelector,Component,0, 0,23,22));
      ATBORDERSelector.Width := 23;
      ATBORDERSelector.Height := 22;
      ATBORDERSelector.Parent := ATB;
      ATBORDERSelector.Caption := '';
      ATBORDERSelector.FocusType := ftHot;
    end;
  19:
    begin
      ACHARSelector := TAdvOfficeCharacterSelector(Designer.CreateComponent(TAdvOfficeCharacterSelector,Component,0, 0,23,22));
      ACHARSelector.Width := 23;
      ACHARSelector.Height := 22;
      ACHARSelector.Parent := ATB;
      ACHARSelector.Caption := '';
      ACHARSelector.FocusType := ftHot;
    end;
  20:
    begin
      ATOOLSelector := TAdvOfficeToolSelector(Designer.CreateComponent(TAdvOfficeToolSelector,Component,0, 0,23,22));
      ATOOLSelector.Width := 23;
      ATOOLSelector.Height := 22;
      ATOOLSelector.Parent := ATB;
      ATOOLSelector.Caption := '';
      ATOOLSelector.FocusType := ftHot;
    end;
  21:
    begin
      AFONTSelector := TAdvOfficeFontSelector(Designer.CreateComponent(TAdvOfficeFontSelector,Component,0, 0,100,22));
      AFONTSelector.Width := 100;
      AFONTSelector.Height := 22;
      AFONTSelector.Parent := ATB;
      //AFONTSelector.Caption := '';
    end;
  22:
    begin
      AFSIZESelector := TAdvOfficeFontSizeSelector(Designer.CreateComponent(TAdvOfficeFontSizeSelector,Component,0, 0,40,22));
      AFSIZESelector.Width := 40;
      AFSIZESelector.Height := 22;
      AFSIZESelector.Parent := ATB;
      //AFONTSelector.Caption := '';
    end;
  23:
    begin
      AFSCROLLSelector := TAdvOfficeScrollSelector(Designer.CreateComponent(TAdvOfficeScrollSelector,Component,0, 0,240,60));
      AFSCROLLSelector.Width := 240;
      AFSCROLLSelector.Height := 60;
      AFSCROLLSelector.Parent := ATB;
    end;

  {$ENDIF}
  end;
end;

function TAdvToolBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Add Button';
  1: Result := 'Add MenuButton';
  2: Result := 'Add Separator';
  3: Result := 'Add DBButton';
  4: Result := 'Add DBNavigator';
  5: Result := 'Add Container';
  6: Result := 'Add AdvGlowButton';
  7: Result := 'Add AdvGlowMenuButton';
  8: Result := 'Add DBAdvGlowButton';
  9: Result := 'Add DBAdvGlowNavigator';
  {$IFDEF TMSPACK}
  10: Result := 'Add AdvOfficeColorSelector';
  11: Result := 'Add AdvOfficeTextColorSelector';
  12: Result := 'Add AdvOfficePenStyleSelector';
  13: Result := 'Add AdvOfficePenWidthSelector';
  14: Result := 'Add AdvOfficeBrushStyleSelector';
  15: Result := 'Add AdvOfficeGradientDirectionSelector';
  16: Result := 'Add AdvOfficeShadowSelector';
  17: Result := 'Add AdvOfficeTableSelector';
  18: Result := 'Add AdvOfficeTableBorderSelector';
  19: Result := 'Add AdvOfficeCharacterSelector';
  20: Result := 'Add AdvOfficeToolSelector';
  21: Result := 'Add AdvOfficeFontSelector';
  22: Result := 'Add AdvOfficeFontSizeSelector';
  23: Result := 'Add AdvOfficeScrollSelector';
  {$ENDIF}
  end;
end;

function TAdvToolBarEditor.GetVerbCount: Integer;
begin
{$IFDEF TMSPACK}
  Result := 24;
{$ELSE}
  Result := 10;
{$ENDIF}
end;


{ TAdvToolBarButtonEditor }

procedure TAdvToolBarButtonEditor.ExecuteVerb(Index: Integer);
var
  ATBButton : TAdvToolBarButton;
  ATBMenuButton: TAdvToolBarMenuButton;
  ATBSeparator: TAdvToolBarSeparator;
  {$IFNDEF TMS_STD}
  DBATBButton: TDBAdvToolBarButton;
  ADBGlowButton: TDBAdvGlowButton;
  {$ENDIF}
  ATB: TWinControl;
  AGlowButton: TAdvGlowButton;
  AGlowMenuButton: TAdvGlowMenuButton;
  ATBContainer: TAdvToolBarContainer;
  I: Integer;
{$IFDEF TMSPACK}
  ACOLSelector: TAdvOfficeColorSelector;
  ATCOLSelector: TAdvOfficeTextColorSelector;
  ASHADSelector: TAdvOfficeShadowSelector;
  AGRADSelector: TAdvOfficeGradientDirectionSelector;
  ATABLESelector: TAdvOfficeTableSelector;
  ATBORDERSelector: TAdvOfficeTableBorderSelector;
  APENSSelector: TAdvOfficePenStyleSelector;
  APENWSelector: TAdvOfficePenWidthSelector;
  ABRUSHSelector: TAdvOfficeBrushStyleSelector;
  ACHARSelector: TAdvOfficeCharacterSelector;
  ATOOLSelector: TAdvOfficeToolSelector;
  AFONTSelector: TAdvOfficeFontSelector;
  AFSIZESelector: TAdvOfficeFontSizeSelector;
  AFSCROLLSelector : TAdvOfficeScrollSelector;
{$ENDIF}

begin
  inherited;

  if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
  begin
    ATB := TAdvToolBarButton(Component).Parent;
    case Index of
    0:
      begin
        ATBButton := TAdvToolBarButton(Designer.CreateComponent(TAdvToolBarButton,Component,0, 0,45,23));
        ATBButton.Width := 23;
        ATBButton.Height := 22;
        ATBButton.Parent := ATB;
        ATBButton.Caption := '';
        if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
        begin
          if Assigned(TAdvToolBar(ATB).Images) then
            ATBButton.ImageIndex := ATBButton.Index;
          ATBButton.AdvToolBar := TAdvToolBar(ATB);
        end
        else if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        begin
          if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
            ATBButton.ImageIndex := ATBButton.Index;
          ATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
        end;
      end;
    1:
      begin
        ATBMenuButton := TAdvToolBarMenuButton(Designer.CreateComponent(TAdvToolBarMenuButton,Component,0, 0,45,23));
        ATBMenuButton.Width := 23;
        ATBMenuButton.Height := 22;
        ATBMenuButton.Parent := ATB;
        ATBMenuButton.Caption := '';
        if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
          ATBMenuButton.AdvToolBar := TAdvToolBar(ATB)
        else if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
          ATBMenuButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
      end;
    2:
      begin
        AGlowButton := TAdvGlowButton(Designer.CreateComponent(TAdvGlowButton,Component,0, 0,23,22));
        AGlowButton.Width := 23;
        AGlowButton.Height := 22;
        AGlowButton.Parent := ATB;
        AGlowButton.Caption := '';

        if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        begin
          AGlowButton.FocusType := ftHot;
          AGlowButton.Width := 22;
          AGlowButton.Height := 20;
          AGlowButton.Transparent := True;
          AGlowButton.Left := 1000;
          if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
            AGlowButton.Images := TImageList(TAdvQuickAccessToolBar(ATB).Images);
          if Assigned(TAdvQuickAccessToolBar(ATB).DisabledImages) then
            AGlowButton.DisabledImages := TImageList(TAdvQuickAccessToolBar(ATB).DisabledImages);
          if (TAdvQuickAccessToolBar(ATB).Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(ATB.Parent).ToolBarStyler) then
            AGlowButton.Appearance.Assign(TAdvToolBarPager(ATB.Parent).ToolBarStyler.GlowButtonAppearance);
        end;
      end;
    3:
      begin
        AGlowMenuButton := TAdvGlowMenuButton(Designer.CreateComponent(TAdvGlowMenuButton,Component,0, 0,23,22));
        AGlowMenuButton.Width := 23;
        AGlowMenuButton.Height := 22;
        AGlowMenuButton.Parent := ATB;
        AGlowMenuButton.Caption := '';

        if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        begin
          AGlowMenuButton.Width := 22;
          AGlowMenuButton.Height := 20;
          AGlowMenuButton.Left := 1000;
          if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
            AGlowMenuButton.Images := TImageList(TAdvQuickAccessToolBar(ATB).Images);
          if Assigned(TAdvQuickAccessToolBar(ATB).DisabledImages) then
            AGlowMenuButton.DisabledImages := TImageList(TAdvQuickAccessToolBar(ATB).DisabledImages);
          if (TAdvQuickAccessToolBar(ATB).Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(ATB.Parent).ToolBarStyler) then
            AGlowMenuButton.Appearance.Assign(TAdvToolBarPager(ATB.Parent).ToolBarStyler.GlowButtonAppearance);
        end;
      end;
    end;
    
    Exit;
  end;
  
  if not (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
    Exit;

  ATB := TAdvToolBarButton(Component).Parent;

  case Index of
  0:
    begin
      ATBButton := TAdvToolBarButton(Designer.CreateComponent(TAdvToolBarButton,Component,0, 0,45,23));
      ATBButton.Width := 23;
      ATBButton.Height := 22;
      ATBButton.Parent := ATB;
      ATBButton.Caption := '';
      if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
      begin
        if Assigned(TAdvToolBar(ATB).Images) then
          ATBButton.ImageIndex := ATBButton.Index;
        ATBButton.AdvToolBar := TAdvToolBar(ATB);
      end
      else if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
          ATBButton.ImageIndex := ATBButton.Index;
        ATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
      end;
    end;
  1:
    begin
      ATBMenuButton := TAdvToolBarMenuButton(Designer.CreateComponent(TAdvToolBarMenuButton,Component,0, 0,45,23));
      ATBMenuButton.Width := 23;
      ATBMenuButton.Height := 22;
      ATBMenuButton.Parent := ATB;
      ATBMenuButton.Caption := ''; //ATBMenuButton.Name;
      if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
        ATBMenuButton.AdvToolBar := TAdvToolBar(ATB)
      else if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        ATBMenuButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
    end;
  2:
    begin
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        Exit;

      ATBSeparator := TAdvToolBarSeparator(Designer.CreateComponent(TAdvToolBarSeparator,Component,0, 0,10,23));
      ATBSeparator.Parent := ATB;
      if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
        ATBSeparator.AdvToolBar := TAdvToolBar(ATB);
    end;
  3:
    begin
      {$IFNDEF TMS_STD}
      DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
      DBATBButton.Width := 23;
      DBATBButton.Height := 22;
      DBATBButton.Parent := ATB;
      DBATBButton.Caption := '';
      if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
        DBATBButton.AdvToolBar := TAdvToolBar(ATB)
      else if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        DBATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
      {$ENDIF}
    end;
  4:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
        DBATBButton.Width := 23;
        DBATBButton.Height := 22;
        DBATBButton.Parent := ATB;
        DBATBButton.Caption := '';
        if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
          DBATBButton.AdvToolBar := TAdvToolBar(ATB)
        else if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
          DBATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
        case I of
          1:
          begin
            DBATBButton.DBButtonType := dbtFirst;
            DBATBButton.Hint := SFirstRecord;
          end;
          2:
          begin
            DBATBButton.DBButtonType := dbtPrior;
            DBATBButton.Hint := SPriorRecord;
          end;
          3:
          begin
            DBATBButton.DBButtonType := dbtNext;
            DBATBButton.Hint := SNextRecord;
          end;
          4:
          begin
            DBATBButton.DBButtonType := dbtLast;
            DBATBButton.Hint := SLastRecord;
          end;
          5:
          begin
            DBATBButton.DBButtonType := dbtInsert;
            DBATBButton.Hint := SInsertRecord;
          end;
          6:
          begin
            DBATBButton.ConfirmAction := True;
            DBATBButton.DBButtonType := dbtDelete;
            DBATBButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            DBATBButton.DBButtonType := dbtEdit;
            DBATBButton.Hint := SEditRecord;
          end;
          8:
          begin
            DBATBButton.DBButtonType := dbtPost;
            DBATBButton.Hint := SPostEdit;
          end;
          9:
          begin
            DBATBButton.DBButtonType := dbtCancel;
            DBATBButton.Hint := SCancelEdit;
          end;
          10:
          begin
            DBATBButton.DBButtonType := dbtRefresh;
            DBATBButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}
    end;
  5:
    begin
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        Exit;

      ATBContainer := TAdvToolBarContainer(Designer.CreateComponent(TAdvToolBarContainer,Component,0, 0,65,45));
      ATBContainer.Width := 65;
      ATBContainer.Height := 45;
      ATBContainer.Parent := ATB;
      //ATBContainer.Caption := '';
      if (TAdvToolBarButton(Component).Parent is TAdvToolBar) then
        ATBContainer.AdvToolBar := TAdvToolBar(ATB);
    end;
  6:
    begin
      AGlowButton := TAdvGlowButton(Designer.CreateComponent(TAdvGlowButton,Component,0, 0,23,22));
      AGlowButton.Width := 23;
      AGlowButton.Height := 22;
      AGlowButton.Parent := ATB;
      AGlowButton.Caption := '';

      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AGlowButton.FocusType := ftHot;
        AGlowButton.Width := 22;
        AGlowButton.Height := 20;
        AGlowButton.Transparent := True;
        AGlowButton.Left := 1000;
        if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
          AGlowButton.Images := TImageList(TAdvQuickAccessToolBar(ATB).Images);
        if Assigned(TAdvQuickAccessToolBar(ATB).DisabledImages) then
          AGlowButton.DisabledImages := TImageList(TAdvQuickAccessToolBar(ATB).DisabledImages);
        if (TAdvQuickAccessToolBar(ATB).Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(ATB.Parent).ToolBarStyler) then
          AGlowButton.Appearance.Assign(TAdvToolBarPager(ATB.Parent).ToolBarStyler.GlowButtonAppearance);
      end;
    end;
  7:
    begin
      AGlowMenuButton := TAdvGlowMenuButton(Designer.CreateComponent(TAdvGlowMenuButton,Component,0, 0,23,22));
      AGlowMenuButton.Width := 23;
      AGlowMenuButton.Height := 22;
      AGlowMenuButton.Parent := ATB;
      AGlowMenuButton.Caption := '';

      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AGlowMenuButton.Width := 22;
        AGlowMenuButton.Height := 20;
        AGlowMenuButton.Left := 1000;
        if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
          AGlowMenuButton.Images := TImageList(TAdvQuickAccessToolBar(ATB).Images);
        if Assigned(TAdvQuickAccessToolBar(ATB).DisabledImages) then
          AGlowMenuButton.DisabledImages := TImageList(TAdvQuickAccessToolBar(ATB).DisabledImages);
        if (TAdvQuickAccessToolBar(ATB).Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(ATB.Parent).ToolBarStyler) then
          AGlowMenuButton.Appearance.Assign(TAdvToolBarPager(ATB.Parent).ToolBarStyler.GlowButtonAppearance);
      end;
    end;
  8:
    begin
      {$IFNDEF TMS_STD}
      ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
      ADBGlowButton.Width := 23;
      ADBGlowButton.Height := 22;
      ADBGlowButton.Parent := ATB;
      ADBGlowButton.Caption := '';

      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ADBGlowButton.Width := 22;
        ADBGlowButton.Height := 20;
        ADBGlowButton.Left := 1000;
        if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
          ADBGlowButton.Images := TImageList(TAdvQuickAccessToolBar(ATB).Images);
        if Assigned(TAdvQuickAccessToolBar(ATB).DisabledImages) then
          ADBGlowButton.DisabledImages := TImageList(TAdvQuickAccessToolBar(ATB).DisabledImages);
        if (TAdvQuickAccessToolBar(ATB).Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(ATB.Parent).ToolBarStyler) then
          ADBGlowButton.Appearance.Assign(TAdvToolBarPager(ATB.Parent).ToolBarStyler.GlowButtonAppearance);
      end;
      {$ENDIF}
    end;
  9:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
        ADBGlowButton.Width := 23;
        ADBGlowButton.Height := 22;
        ADBGlowButton.Parent := ATB;
        ADBGlowButton.Caption := '';

        if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        begin
          ADBGlowButton.Width := 22;
          ADBGlowButton.Height := 20;
          ADBGlowButton.Left := 1000;
          if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
            ADBGlowButton.Images := TImageList(TAdvQuickAccessToolBar(ATB).Images);
          if Assigned(TAdvQuickAccessToolBar(ATB).DisabledImages) then
            ADBGlowButton.DisabledImages := TImageList(TAdvQuickAccessToolBar(ATB).DisabledImages);
          if (TAdvQuickAccessToolBar(ATB).Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(ATB.Parent).ToolBarStyler) then
            ADBGlowButton.Appearance.Assign(TAdvToolBarPager(ATB.Parent).ToolBarStyler.GlowButtonAppearance);
        end;

        case I of
          1:
          begin
            ADBGlowButton.DBButtonType := dbFirst;
            ADBGlowButton.Hint := SFirstRecord;
          end;
          2:
          begin
            ADBGlowButton.DBButtonType := dbPrior;
            ADBGlowButton.Hint := SPriorRecord;
          end;
          3:
          begin
            ADBGlowButton.DBButtonType := dbNext;
            ADBGlowButton.Hint := SNextRecord;
          end;
          4:
          begin
            ADBGlowButton.DBButtonType := dbLast;
            ADBGlowButton.Hint := SLastRecord;
          end;
          5:
          begin
            ADBGlowButton.DBButtonType := dbInsert;
            ADBGlowButton.Hint := SInsertRecord;
          end;
          6:
          begin
            ADBGlowButton.ConfirmAction := True;
            ADBGlowButton.DBButtonType := dbDelete;
            ADBGlowButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            ADBGlowButton.DBButtonType := dbEdit;
            ADBGlowButton.Hint := SEditRecord;
          end;
          8:
          begin
            ADBGlowButton.DBButtonType := dbPost;
            ADBGlowButton.Hint := SPostEdit;
          end;
          9:
          begin
            ADBGlowButton.DBButtonType := dbCancel;
            ADBGlowButton.Hint := SCancelEdit;
          end;
          10:
          begin
            ADBGlowButton.DBButtonType := dbRefresh;
            ADBGlowButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}
    end;
  {$IFDEF TMSPACK}
  10:
    begin
      ACOLSelector := TAdvOfficeColorSelector(Designer.CreateComponent(TAdvOfficeColorSelector,Component,0, 0,23,22));
      ACOLSelector.Width := 23;
      ACOLSelector.Height := 22;
      ACOLSelector.Parent := ATB;
      ACOLSelector.Caption := '';
      AColSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ACOLSelector.Width := 22;
        ACOLSelector.Height := 20;
        ACOLSelector.Left := 1000;
      end;
    end;
  11:
    begin
      ATCOLSelector := TAdvOfficeTextColorSelector(Designer.CreateComponent(TAdvOfficeTextColorSelector,Component,0, 0,23,22));
      ATCOLSelector.Width := 23;
      ATCOLSelector.Height := 22;
      ATCOLSelector.Parent := ATB;
      ATCOLSelector.Caption := '';
      ATCOLSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATCOLSelector.Width := 22;
        ATCOLSelector.Height := 20;
        ATCOLSelector.Left := 1000;
      end;
    end;
  12:
    begin
      APENSSelector := TAdvOfficePenStyleSelector(Designer.CreateComponent(TAdvOfficePenStyleSelector,Component,0, 0,23,22));
      APENSSelector.Width := 23;
      APENSSelector.Height := 22;
      APENSSelector.Parent := ATB;
      APENSSelector.Caption := '';
      APENSSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        APENSSelector.Width := 22;
        APENSSelector.Height := 20;
        APENSSelector.Left := 1000;
      end;
    end;
  13:
    begin
      APENWSelector := TAdvOfficePenWidthSelector(Designer.CreateComponent(TAdvOfficePenWidthSelector,Component,0, 0,23,22));
      APENWSelector.Width := 23;
      APENWSelector.Height := 22;
      APENWSelector.Parent := ATB;
      APENWSelector.Caption := '';
      APENWSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        APENWSelector.Width := 22;
        APENWSelector.Height := 20;
        APENWSelector.Left := 1000;
      end;
    end;
  14:
    begin
      ABRUSHSelector := TAdvOfficeBrushStyleSelector(Designer.CreateComponent(TAdvOfficeBrushStyleSelector,Component,0, 0,23,22));
      ABRUSHSelector.Width := 23;
      ABRUSHSelector.Height := 22;
      ABRUSHSelector.Parent := ATB;
      ABRUSHSelector.Caption := '';
      ABRUSHSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ABRUSHSelector.Width := 22;
        ABRUSHSelector.Height := 20;
        ABRUSHSelector.Left := 1000;
      end;
    end;
  15:
    begin
      AGRADSelector := TAdvOfficeGradientDirectionSelector(Designer.CreateComponent(TAdvOfficeGradientDirectionSelector,Component,0, 0,23,22));
      AGRADSelector.Width := 23;
      AGRADSelector.Height := 22;
      AGRADSelector.Parent := ATB;
      AGRADSelector.Caption := '';
      AGRADSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AGRADSelector.Width := 22;
        AGRADSelector.Height := 20;
        AGRADSelector.Left := 1000;
      end;
    end;
  16:
    begin
      ASHADSelector := TAdvOfficeShadowSelector(Designer.CreateComponent(TAdvOfficeShadowSelector,Component,0, 0,23,22));
      ASHADSelector.Width := 23;
      ASHADSelector.Height := 22;
      ASHADSelector.Parent := ATB;
      ASHADSelector.Caption := '';
      ASHADSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ASHADSelector.Width := 22;
        ASHADSelector.Height := 20;
        ASHADSelector.Left := 1000;
      end;
    end;
  17:
    begin
      ATABLESelector := TAdvOfficeTableSelector(Designer.CreateComponent(TAdvOfficeTableSelector,Component,0, 0,23,22));
      ATABLESelector.Width := 23;
      ATABLESelector.Height := 22;
      ATABLESelector.Parent := ATB;
      ATABLESelector.Caption := '';
      ATABLESelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATABLESelector.Width := 22;
        ATABLESelector.Height := 20;
        ATABLESelector.Left := 1000;
      end;
    end;
  18:
    begin
      ATBORDERSelector := TAdvOfficeTableBorderSelector(Designer.CreateComponent(TAdvOfficeTableBorderSelector,Component,0, 0,23,22));
      ATBORDERSelector.Width := 23;
      ATBORDERSelector.Height := 22;
      ATBORDERSelector.Parent := ATB;
      ATBORDERSelector.Caption := '';
      ATBORDERSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATBORDERSelector.Width := 22;
        ATBORDERSelector.Height := 20;
        ATBORDERSelector.Left := 1000;
      end;
    end;
  19:
    begin
      ACHARSelector := TAdvOfficeCharacterSelector(Designer.CreateComponent(TAdvOfficeCharacterSelector,Component,0, 0,23,22));
      ACHARSelector.Width := 23;
      ACHARSelector.Height := 22;
      ACHARSelector.Parent := ATB;
      ACHARSelector.Caption := '';
      ACHARSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ACHARSelector.Width := 22;
        ACHARSelector.Height := 20;
        ACHARSelector.Left := 1000;
      end;
    end;
  20:
    begin
      ATOOLSelector := TAdvOfficeToolSelector(Designer.CreateComponent(TAdvOfficeToolSelector,Component,0, 0,23,22));
      ATOOLSelector.Width := 23;
      ATOOLSelector.Height := 22;
      ATOOLSelector.Parent := ATB;
      ATOOLSelector.Caption := '';
      ATOOLSelector.FocusType := ftHot;
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATOOLSelector.Width := 22;
        ATOOLSelector.Height := 20;
        ATOOLSelector.Left := 1000;
      end;
    end;
  21:
    begin
      AFONTSelector := TAdvOfficeFontSelector(Designer.CreateComponent(TAdvOfficeFontSelector,Component,0, 0,100,22));
      AFONTSelector.Width := 100;
      AFONTSelector.Height := 22;
      AFONTSelector.Parent := ATB;
      //AFONTSelector.Caption := '';
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AFONTSelector.Width := 22;
        AFONTSelector.Height := 20;
        AFONTSelector.Left := 1000;
      end;
    end;
  22:
    begin
      AFSIZESelector := TAdvOfficeFontSizeSelector(Designer.CreateComponent(TAdvOfficeFontSizeSelector,Component,0, 0,40,22));
      AFSIZESelector.Width := 40;
      AFSIZESelector.Height := 22;
      AFSIZESelector.Parent := ATB;
      //AFONTSelector.Caption := '';
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AFSIZESelector.Width := 22;
        AFSIZESelector.Height := 20;
        AFSIZESelector.Left := 1000;
      end;
    end;
  23:
    begin
      if (TAdvToolBarButton(Component).Parent is TAdvQuickAccessToolBar) then
        Exit;
        
      AFSCROLLSelector := TAdvOfficeScrollSelector(Designer.CreateComponent(TAdvOfficeScrollSelector,Component,0, 0,240,60));
      AFSCROLLSelector.Width := 240;
      AFSCROLLSelector.Height := 60;
      AFSCROLLSelector.Parent := ATB;
    end;
  {$ENDIF}
  end;
end;

function TAdvToolBarButtonEditor.GetVerb(Index: Integer): string;
begin
  if (Component is TAdvToolBarButton) then
  begin
    if ((Component as TAdvToolBarButton).Parent is TAdvQuickAccessToolBar) then
    begin
      case Index of
      0: Result := 'Add Button';
      1: Result := 'Add MenuButton';
      2: Result := 'Add AdvGlowButton';
      3: Result := 'Add AdvGlowMenuButton';
      end;
      Exit;
    end;
  end;
  case Index of
  0: Result := 'Add Button';
  1: Result := 'Add MenuButton';
  2: Result := 'Add Separator';
  3: Result := 'Add DBButton';
  4: Result := 'Add DBNavigator';
  5: Result := 'Add Container';
  6: Result := 'Add AdvGlowButton';
  7: Result := 'Add AdvGlowMenuButton';
  8: Result := 'Add DBAdvGlowButton';
  9: Result := 'Add DBAdvGlowNavigator';
  {$IFDEF TMSPACK}
  10: Result := 'Add AdvOfficeColorSelector';
  11: Result := 'Add AdvOfficeTextColorSelector';
  12: Result := 'Add AdvOfficePenStyleSelector';
  13: Result := 'Add AdvOfficePenWidthSelector';
  14: Result := 'Add AdvOfficeBrushStyleSelector';
  15: Result := 'Add AdvOfficeGradientDirectionSelector';
  16: Result := 'Add AdvOfficeShadowSelector';
  17: Result := 'Add AdvOfficeTableSelector';
  18: Result := 'Add AdvOfficeTableBorderSelector';
  19: Result := 'Add AdvOfficeCharacterSelector';
  20: Result := 'Add AdvOfficeToolSelector';
  21: Result := 'Add AdvOfficeFontSelector';
  22: Result := 'Add AdvOfficeFontSizeSelector';
  23: Result := 'Add AdvOfficeScrollSelector';
  {$ENDIF}
  end;
end;

function TAdvToolBarButtonEditor.GetVerbCount: Integer;
begin
  if (Component is TAdvToolBarButton) then
  begin
    if ((Component as TAdvToolBarButton).Parent is TAdvQuickAccessToolBar) then
    begin
      Result := 4;
      Exit;
    end;
  end;
{$IFDEF TMSPACK}
  Result := 24;
{$ELSE}
  Result := 10;
{$ENDIF}
end;

{ TAdvDockPanelEditor }

procedure TAdvDockPanelEditor.ExecuteVerb(Index: Integer);
var
  AToolBar: TAdvToolBar;
  ADockPanel: TAdvDockPanel;
begin
  inherited;

  ADockPanel := TAdvDockPanel(Component);

  case Index of
  0:
    begin
      if ADockPanel.Align in [daTop, daBottom] then
      begin
        AToolBar := TAdvToolBar(Designer.CreateComponent(TAdvToolBar,Component,0, 0,45,23));
        AToolBar.Width := 120;
        AToolBar.Height := 26;
      end
      else
      begin
        AToolBar := TAdvToolBar(Designer.CreateComponent(TAdvToolBar,Component,0, 0,23,45));
        AToolBar.Width := 26;
        AToolBar.Height := 120;
      end;
      AToolBar.Parent := ADockPanel;
      AToolBar.Caption := 'Untitled';
    end;
  end;
end;

function TAdvDockPanelEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Add ToolBar';
  end;
end;

function TAdvDockPanelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TAdvToolBarContainerEditor }

procedure TAdvToolBarContainerEditor.ExecuteVerb(Index: Integer);
var
  ATBButton : TAdvToolBarButton;
  ATBMenuButton: TAdvToolBarMenuButton;
  ATBSeparator: TAdvToolBarSeparator;
  {$IFNDEF TMS_STD}
  DBATBButton: TDBAdvToolBarButton;
  ADBGlowButton: TDBAdvGlowButton;
  {$ENDIF}
  ATB: TAdvToolBar;
  AGlowButton: TAdvGlowButton;
  AGlowMenuButton: TAdvGlowMenuButton;

  I: Integer;
{$IFDEF TMSPACK}
  ACOLSelector: TAdvOfficeColorSelector;
  ATCOLSelector: TAdvOfficeTextColorSelector;
  ASHADSelector: TAdvOfficeShadowSelector;
  AGRADSelector: TAdvOfficeGradientDirectionSelector;
  ATABLESelector: TAdvOfficeTableSelector;
  ATBORDERSelector: TAdvOfficeTableBorderSelector;
  APENSSelector: TAdvOfficePenStyleSelector;
  APENWSelector: TAdvOfficePenWidthSelector;
  ABRUSHSelector: TAdvOfficeBrushStyleSelector;
  ACHARSelector: TAdvOfficeCharacterSelector;
  ATOOLSelector: TAdvOfficeToolSelector;
  AFONTSelector: TAdvOfficeFontSelector;  
  AFSIZESelector: TAdvOfficeFontSizeSelector;
  AFSCROLLSelector : TAdvOfficeScrollSelector;
{$ENDIF}


begin
  inherited;
  if not (TAdvToolBarContainer(Component).Parent is TAdvToolBar) then
    Exit;

  ATB := TAdvToolBar(TAdvToolBarContainer(Component).Parent);

  case Index of
  0:
    begin
      ATBButton := TAdvToolBarButton(Designer.CreateComponent(TAdvToolBarButton,Component,0, 0,45,23));
      ATBButton.Width := 23;
      ATBButton.Height := 22;
      ATBButton.AutoSize := False;
      ATBButton.Parent := TAdvToolBarContainer(Component);
      ATBButton.Caption := '';
      if Assigned(ATB.Images) then
        ATBButton.ImageIndex := ATBButton.Index;
      //ATBButton.AdvToolBar := ATB;
    end;
  1:
    begin
      ATBMenuButton := TAdvToolBarMenuButton(Designer.CreateComponent(TAdvToolBarMenuButton,Component,0, 0,45,23));
      ATBMenuButton.Width := 23;
      ATBMenuButton.Height := 22;
      ATBMenuButton.Parent := TAdvToolBarContainer(Component);
      ATBMenuButton.Caption := ''; //ATBMenuButton.Name;
      //ATBMenuButton.AdvToolBar := ATB;
    end;
  2:
    begin
      ATBSeparator := TAdvToolBarSeparator(Designer.CreateComponent(TAdvToolBarSeparator,Component,0, 0,10,23));
      ATBSeparator.Parent := TAdvToolBarContainer(Component);
      //ATBSeparator.AdvToolBar := ATB;
    end;
  3:
    begin
      {$IFNDEF TMS_STD}
      DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
      DBATBButton.Width := 23;
      DBATBButton.Height := 22;
      DBATBButton.AutoSize := False;
      DBATBButton.Parent := TAdvToolBarContainer(Component);
      DBATBButton.Caption := '';
      //DBATBButton.AdvToolBar := ATB;
      {$ENDIF}
    end;
  4:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
        DBATBButton.Width := 23;
        DBATBButton.Height := 22;
        DBATBButton.AutoSize := False;
        DBATBButton.Parent := TAdvToolBarContainer(Component);
        DBATBButton.Caption := '';
        //DBATBButton.AdvToolBar := ATB;
        case I of
          1:
          begin
            DBATBButton.DBButtonType := dbtFirst;
            DBATBButton.Hint := SFirstRecord;
          end;
          2:
          begin
            DBATBButton.DBButtonType := dbtPrior;
            DBATBButton.Hint := SPriorRecord;
          end;
          3:
          begin
            DBATBButton.DBButtonType := dbtNext;
            DBATBButton.Hint := SNextRecord;
          end;
          4:
          begin
            DBATBButton.DBButtonType := dbtLast;
            DBATBButton.Hint := SLastRecord;
          end;
          5:
          begin
            DBATBButton.DBButtonType := dbtInsert;
            DBATBButton.Hint := SInsertRecord;
          end;
          6:
          begin
            DBATBButton.ConfirmAction := True;
            DBATBButton.DBButtonType := dbtDelete;
            DBATBButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            DBATBButton.DBButtonType := dbtEdit;
            DBATBButton.Hint := SEditRecord;
          end;
          8:
          begin
            DBATBButton.DBButtonType := dbtPost;
            DBATBButton.Hint := SPostEdit;
          end;
          9:
          begin
            DBATBButton.DBButtonType := dbtCancel;
            DBATBButton.Hint := SCancelEdit;
          end;
          10:
          begin
            DBATBButton.DBButtonType := dbtRefresh;
            DBATBButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}
    end;
  5:
    begin
      AGlowButton := TAdvGlowButton(Designer.CreateComponent(TAdvGlowButton,Component,0, 0,23,22));
      AGlowButton.Width := 23;
      AGlowButton.Height := 22;
      AGlowButton.Parent := TAdvToolBarContainer(Component);
      AGlowButton.Caption := '';
    end;
  6:
    begin
      AGlowMenuButton := TAdvGlowMenuButton(Designer.CreateComponent(TAdvGlowMenuButton,Component,0, 0,23,22));
      AGlowMenuButton.Width := 23;
      AGlowMenuButton.Height := 22;
      AGlowMenuButton.Parent := TAdvToolBarContainer(Component);
      AGlowMenuButton.Caption := '';
    end;
  7:
    begin
      {$IFNDEF TMS_STD}
      ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
      ADBGlowButton.Width := 23;
      ADBGlowButton.Height := 22;
      ADBGlowButton.Parent := ATB;
      ADBGlowButton.Caption := '';
      {$ENDIF}
    end;
  8:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
        ADBGlowButton.Width := 23;
        ADBGlowButton.Height := 22;
        ADBGlowButton.Parent := ATB;
        ADBGlowButton.Caption := '';
        case I of
          1:
          begin
            ADBGlowButton.DBButtonType := dbFirst;
            ADBGlowButton.Hint := SFirstRecord;
          end;
          2:
          begin
            ADBGlowButton.DBButtonType := dbPrior;
            ADBGlowButton.Hint := SPriorRecord;
          end;
          3:
          begin
            ADBGlowButton.DBButtonType := dbNext;
            ADBGlowButton.Hint := SNextRecord;
          end;
          4:
          begin
            ADBGlowButton.DBButtonType := dbLast;
            ADBGlowButton.Hint := SLastRecord;
          end;
          5:
          begin
            ADBGlowButton.DBButtonType := dbInsert;
            ADBGlowButton.Hint := SInsertRecord;
          end;
          6:
          begin
            ADBGlowButton.ConfirmAction := True;
            ADBGlowButton.DBButtonType := dbDelete;
            ADBGlowButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            ADBGlowButton.DBButtonType := dbEdit;
            ADBGlowButton.Hint := SEditRecord;
          end;
          8:
          begin
            ADBGlowButton.DBButtonType := dbPost;
            ADBGlowButton.Hint := SPostEdit;
          end;
          9:
          begin
            ADBGlowButton.DBButtonType := dbCancel;
            ADBGlowButton.Hint := SCancelEdit;
          end;
          10:
          begin
            ADBGlowButton.DBButtonType := dbRefresh;
            ADBGlowButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}
    end;
  {$IFDEF TMSPACK}
  9:
    begin
      ACOLSelector := TAdvOfficeColorSelector(Designer.CreateComponent(TAdvOfficeColorSelector,Component,0, 0,23,22));
      ACOLSelector.Width := 23;
      ACOLSelector.Height := 22;
      ACOLSelector.Parent := ATB;
      ACOLSelector.Caption := '';
      AColSelector.FocusType := ftHot;
    end;
  10:
    begin
      ATCOLSelector := TAdvOfficeTextColorSelector(Designer.CreateComponent(TAdvOfficeTextColorSelector,Component,0, 0,23,22));
      ATCOLSelector.Width := 23;
      ATCOLSelector.Height := 22;
      ATCOLSelector.Parent := ATB;
      ATCOLSelector.Caption := '';
      ATCOLSelector.FocusType := ftHot;
    end;
  11:
    begin
      APENSSelector := TAdvOfficePenStyleSelector(Designer.CreateComponent(TAdvOfficePenStyleSelector,Component,0, 0,23,22));
      APENSSelector.Width := 23;
      APENSSelector.Height := 22;
      APENSSelector.Parent := ATB;
      APENSSelector.Caption := '';
      APENSSelector.FocusType := ftHot;
    end;
  12:
    begin
      APENWSelector := TAdvOfficePenWidthSelector(Designer.CreateComponent(TAdvOfficePenWidthSelector,Component,0, 0,23,22));
      APENWSelector.Width := 23;
      APENWSelector.Height := 22;
      APENWSelector.Parent := ATB;
      APENWSelector.Caption := '';
      APENWSelector.FocusType := ftHot;
    end;
  13:
    begin
      ABRUSHSelector := TAdvOfficeBrushStyleSelector(Designer.CreateComponent(TAdvOfficeBrushStyleSelector,Component,0, 0,23,22));
      ABRUSHSelector.Width := 23;
      ABRUSHSelector.Height := 22;
      ABRUSHSelector.Parent := ATB;
      ABRUSHSelector.Caption := '';
      ABRUSHSelector.FocusType := ftHot;
    end;
  14:
    begin
      AGRADSelector := TAdvOfficeGradientDirectionSelector(Designer.CreateComponent(TAdvOfficeGradientDirectionSelector,Component,0, 0,23,22));
      AGRADSelector.Width := 23;
      AGRADSelector.Height := 22;
      AGRADSelector.Parent := ATB;
      AGRADSelector.Caption := '';
      AGRADSelector.FocusType := ftHot;
    end;
  15:
    begin
      ASHADSelector := TAdvOfficeShadowSelector(Designer.CreateComponent(TAdvOfficeShadowSelector,Component,0, 0,23,22));
      ASHADSelector.Width := 23;
      ASHADSelector.Height := 22;
      ASHADSelector.Parent := ATB;
      ASHADSelector.Caption := '';
      ASHADSelector.FocusType := ftHot;
    end;
  16:
    begin
      ATABLESelector := TAdvOfficeTableSelector(Designer.CreateComponent(TAdvOfficeTableSelector,Component,0, 0,23,22));
      ATABLESelector.Width := 23;
      ATABLESelector.Height := 22;
      ATABLESelector.Parent := ATB;
      ATABLESelector.Caption := '';
      ATABLESelector.FocusType := ftHot;
    end;
  17:
    begin
      ATBORDERSelector := TAdvOfficeTableBorderSelector(Designer.CreateComponent(TAdvOfficeTableBorderSelector,Component,0, 0,23,22));
      ATBORDERSelector.Width := 23;
      ATBORDERSelector.Height := 22;
      ATBORDERSelector.Parent := ATB;
      ATBORDERSelector.Caption := '';
      ATBORDERSelector.FocusType := ftHot;
    end;
  18:
    begin
      ACHARSelector := TAdvOfficeCharacterSelector(Designer.CreateComponent(TAdvOfficeCharacterSelector,Component,0, 0,23,22));
      ACHARSelector.Width := 23;
      ACHARSelector.Height := 22;
      ACHARSelector.Parent := ATB;
      ACHARSelector.Caption := '';
      ACHARSelector.FocusType := ftHot;
    end;
  19:
    begin
      ATOOLSelector := TAdvOfficeToolSelector(Designer.CreateComponent(TAdvOfficeToolSelector,Component,0, 0,23,22));
      ATOOLSelector.Width := 23;
      ATOOLSelector.Height := 22;
      ATOOLSelector.Parent := ATB;
      ATOOLSelector.Caption := '';
      ATOOLSelector.FocusType := ftHot;
    end;
  20:
    begin
      AFONTSelector := TAdvOfficeFontSelector(Designer.CreateComponent(TAdvOfficeFontSelector,Component,0, 0,100,22));
      AFONTSelector.Width := 100;
      AFONTSelector.Height := 22;
      AFONTSelector.Parent := ATB;
      //AFONTSelector.Caption := '';
    end;
  21:
    begin
      AFSIZESelector := TAdvOfficeFontSizeSelector(Designer.CreateComponent(TAdvOfficeFontSizeSelector,Component,0, 0,40,22));
      AFSIZESelector.Width := 40;
      AFSIZESelector.Height := 22;
      AFSIZESelector.Parent := ATB;
      //AFONTSelector.Caption := '';
    end;
  22:
    begin
      AFSCROLLSelector := TAdvOfficeScrollSelector(Designer.CreateComponent(TAdvOfficeScrollSelector,Component,0, 0,240,60));
      AFSCROLLSelector.Width := 240;
      AFSCROLLSelector.Height := 60;
      AFSCROLLSelector.Parent := ATB;
    end;
  {$ENDIF}
  end;
end;

function TAdvToolBarContainerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Add Button';
  1: Result := 'Add MenuButton';
  2: Result := 'Add Separator';
  3: Result := 'Add DBButton';
  4: Result := 'Add DBNavigator';
  5: Result := 'Add AdvGlowButton';
  6: Result := 'Add AdvGlowMenuButton';
  7: Result := 'Add DBAdvGlowButton';
  8: Result := 'Add DBAdvGlowNavigator';
  {$IFDEF TMSPACK}
  9: Result := 'Add AdvOfficeColorSelector';
  10: Result := 'Add AdvOfficeTextColorSelector';
  11: Result := 'Add AdvOfficePenStyleSelector';
  12: Result := 'Add AdvOfficePenWidthSelector';
  13: Result := 'Add AdvOfficeBrushStyleSelector';
  14: Result := 'Add AdvOfficeGradientDirectionSelector';
  15: Result := 'Add AdvOfficeShadowSelector';
  16: Result := 'Add AdvOfficeTableSelector';
  17: Result := 'Add AdvOfficeTableBorderSelector';
  18: Result := 'Add AdvOfficeCharacterSelector';
  19: Result := 'Add AdvOfficeToolSelector';
  20: Result := 'Add AdvOfficeFontSelector';
  21: Result := 'Add AdvOfficeFontSizeSelector';
  22: Result := 'Add AdvOfficeScrollSelector';
  {$ENDIF}
  end;
end;

function TAdvToolBarContainerEditor.GetVerbCount: Integer;
begin
{$IFDEF TMSPACK}
  Result := 23;
{$ELSE}
  Result := 9;
{$ENDIF}
end;


{ TAdvGlowButtonEditor }

procedure TAdvGlowButtonEditor.ExecuteVerb(Index: Integer);
var
  ATBButton : TAdvToolBarButton;
  ATBMenuButton: TAdvToolBarMenuButton;
  ATBSeparator: TAdvToolBarSeparator;
  {$IFNDEF TMS_STD}
  DBATBButton: TDBAdvToolBarButton;
  ADBGlowButton: TDBAdvGlowButton;
  {$ENDIF}
  ATB: TWinControl;
  AGlowButton: TAdvGlowButton;
  AGlowMenuButton: TAdvGlowMenuButton;

  ATBContainer: TAdvToolBarContainer;
  I: Integer;
{$IFDEF TMSPACK}
  ACOLSelector: TAdvOfficeColorSelector;
  ATCOLSelector: TAdvOfficeTextColorSelector;
  ASHADSelector: TAdvOfficeShadowSelector;
  AGRADSelector: TAdvOfficeGradientDirectionSelector;
  ATABLESelector: TAdvOfficeTableSelector;
  ATBORDERSelector: TAdvOfficeTableBorderSelector;
  APENSSelector: TAdvOfficePenStyleSelector;
  APENWSelector: TAdvOfficePenWidthSelector;
  ABRUSHSelector: TAdvOfficeBrushStyleSelector;
  ACHARSelector: TAdvOfficeCharacterSelector;
  ATOOLSelector: TAdvOfficeToolSelector;
  AFONTSelector: TAdvOfficeFontSelector;  
  AFSIZESelector: TAdvOfficeFontSizeSelector;
  AFSCROLLSelector : TAdvOfficeScrollSelector;
{$ENDIF}
begin
  inherited;

  if (Component is TAdvCustomGlowButton) then
  begin
    if ((Component as TAdvCustomGlowButton).Parent is TAdvQuickAccessToolBar) then
    begin
      ATB := TAdvCustomGlowButton(Component).Parent;
      
      case Index of
      0:
        begin
          ATBButton := TAdvToolBarButton(Designer.CreateComponent(TAdvToolBarButton,Component,0, 0,45,23));
          ATBButton.Width := 23;
          ATBButton.Height := 22;
          ATBButton.Parent := ATB;
          ATBButton.Caption := '';
          if (TAdvCustomGlowButton(Component).Parent is TAdvToolBar) then
          begin
            if Assigned(TAdvToolBar(ATB).Images) then
              ATBButton.ImageIndex := ATBButton.Index;
            ATBButton.AdvToolBar := TAdvToolBar(ATB);
          end
          else if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
          begin
            ATBButton.Width := 22;
            ATBButton.Height := 20;
            ATBButton.Left := 1000;
            if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
              ATBButton.ImageIndex := ATBButton.Index;
            ATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
          end;
        end;
      1:
        begin
          ATBMenuButton := TAdvToolBarMenuButton(Designer.CreateComponent(TAdvToolBarMenuButton,Component,0, 0,45,23));
          ATBMenuButton.Width := 23;
          ATBMenuButton.Height := 22;
          ATBMenuButton.Parent := ATB;
          ATBMenuButton.Caption := '';
          if (TAdvCustomGlowButton(Component).Parent is TAdvToolBar) then
            ATBMenuButton.AdvToolBar := TAdvToolBar(ATB)
          else if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
          begin
            ATBMenuButton.Width := 22;
            ATBMenuButton.Height := 20;
            ATBMenuButton.Left := 1000;
            ATBMenuButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
          end;
        end;
      2:
        begin
          AGlowButton := TAdvGlowButton(Designer.CreateComponent(TAdvGlowButton,Component,0, 0,23,22));
          AGlowButton.Width := 23;
          AGlowButton.Height := 22;
          AGlowButton.Parent := ATB;
          AGlowButton.Caption := '';

          if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
          begin
            AGlowButton.Width := 22;
            AGlowButton.Height := 20;
            AGlowButton.Transparent := True;
            AGlowButton.Left := 1000;
          end;
        end;
      3:
        begin
          AGlowMenuButton := TAdvGlowMenuButton(Designer.CreateComponent(TAdvGlowMenuButton,Component,0, 0,23,22));
          AGlowMenuButton.Width := 23;
          AGlowMenuButton.Height := 22;
          AGlowMenuButton.Parent := ATB;
          AGlowMenuButton.Caption := '';
          if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
          begin
            AGlowMenuButton.Left := 1000;
          end;
        end;
      end;

      Exit;
    end
    else if not ((Component as TAdvCustomGlowButton).Parent is TAdvToolBar) then
    begin
      case Index of
      0:
        begin
          with (Component as TAdvCustomGlowButton).Appearance do
          begin
            Color := clWhite;
            ColorTo := clWhite;
            ColorMirror := clSilver;
            ColorMirrorTo := clWhite;

            ColorHot := $F5F0E1;
            ColorHotTo := $F9D2B2;
            ColorMirrorHot := $F5C8AD;
            ColorMirrorHotTo := $FFF8F4;

            ColorDown := BrightnessColor($F5F0E1,-10,-10,0);
            ColorDownTo := BrightnessColor($F9D2B2, -10,-10,0);
            ColorMirrorDown := BrightnessColor($F5C8AD, -10,-10,0);
            ColorMirrorDownTo := BrightnessColor($FFF8F4, -10,-10,0);

            ColorChecked := BrightnessColor($F5F0E1,-10,-10,0);
            ColorCheckedTo := BrightnessColor($F9D2B2, -10,-10,0);
            ColorMirrorChecked := BrightnessColor($F5C8AD, -10,-10,0);
            ColorMirrorCheckedTo := BrightnessColor($FFF8F4, -10,-10,0);

            ColorDisabled := BrightnessColor(clWhite,-5,-5,-5);
            ColorDisabledTo := BrightnessColor(clWhite, -5,-5,-5);
            ColorMirrorDisabled := BrightnessColor(clSilver, -5,-5,-5);
            ColorMirrorDisabledTo := BrightnessColor(clWhite, -5,-5,-5);

            BorderColor := clSilver;
            BorderColorHot := clBlue;
            BorderColorDown := clNavy;
            BorderColorChecked := clBlue;
            BorderColorDisabled := clGray;

            Gradient := ggVertical;
            GradientMirror := ggVertical;

            GradientHot := ggRadial;
            GradientMirrorHot := ggRadial;

            GradientDown := ggRadial;
            GradientMirrorDown := ggRadial;

            GradientChecked := ggRadial;
            GradientMirrorChecked := ggVertical;

            GradientDisabled := ggRadial;
            GradientMirrorDisabled := ggRadial;

          end;

        end;
      1:
        begin
          with (Component as TAdvCustomGlowButton).Appearance do
          begin
            Color := $EEDBC8;
            ColorTo := $F6DDC9;
            ColorMirror := $EDD4C0;
            ColorMirrorTo := $F7E1D0;
            BorderColor := $E0B99B;
            Gradient := ggVertical;
            GradientMirror := ggVertical;

            ColorHot := $EBFDFF;
            ColorHotTo := $ACECFF;
            ColorMirrorHot := $59DAFF;
            ColorMirrorHotTo := $A4E9FF;
            BorderColorHot := $99CEDB;
            GradientHot := ggVertical;
            GradientMirrorHot := ggVertical;

            ColorDown := $76AFF1;
            ColorDownTo := $4190F3;
            ColorMirrorDown := $0E72F1;
            ColorMirrorDownTo := $4C9FFD;
            BorderColorDown := $45667B;
            GradientDown := ggVertical;
            GradientMirrorDown := ggVertical;


            ColorChecked := $78C7FE;
            ColorCheckedTo := $B5DBFB;
            ColorMirrorChecked := $9FEBFD;
            ColorMirrorCheckedTo := $56B4FE;
            GradientChecked := ggVertical;
            GradientMirrorChecked := ggVertical;
          end;

        end;
      2:
        begin
          with (Component as TAdvCustomGlowButton).Appearance do
          begin
            Color := HTMLToRgb($D6DEDF);
            ColorTo := HTMLToRgb($DBE2E4);
            ColorMirror := HTMLToRgb($CED5D7);
            ColorMirrorTo := HTMLToRgb($E0E5E7);
            BorderColor := HTMLToRgb($B2BCC0);
            Gradient := ggVertical;
            GradientMirror := ggVertical;

            ColorHot := $EBFDFF;
            ColorHotTo := $ACECFF;
            ColorMirrorHot := $59DAFF;
            ColorMirrorHotTo := $A4E9FF;
            BorderColorHot := $99CEDB;
            GradientHot := ggVertical;
            GradientMirrorHot := ggVertical;

            ColorDown := $76AFF1;
            ColorDownTo := $4190F3;
            ColorMirrorDown := $0E72F1;
            ColorMirrorDownTo := $4C9FFD;
            BorderColorDown := $45667B;
            GradientDown := ggVertical;
            GradientMirrorDown := ggVertical;

            ColorChecked := $78C7FE;
            ColorCheckedTo := $B5DBFB;
            ColorMirrorChecked := $9FEBFD;
            ColorMirrorCheckedTo := $56B4FE;
            GradientChecked := ggVertical;
            GradientMirrorChecked := ggVertical;
          end;

        end;
      end;
      (Component as TAdvCustomGlowButton).Repaint;
      Exit;
    end;
  end;

  ATB := TAdvCustomGlowButton(Component).Parent;

  case Index of
  0:
    begin
      ATBButton := TAdvToolBarButton(Designer.CreateComponent(TAdvToolBarButton,Component,0, 0,45,23));
      ATBButton.Width := 23;
      ATBButton.Height := 22;
      ATBButton.Parent := ATB;
      ATBButton.Caption := '';
      if (TAdvCustomGlowButton(Component).Parent is TAdvToolBar) then
      begin
        if Assigned(TAdvToolBar(ATB).Images) then
          ATBButton.ImageIndex := ATBButton.Index;
        ATBButton.AdvToolBar := TAdvToolBar(ATB);
      end
      else if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATBButton.Width := 22;
        ATBButton.Height := 20;
        ATBButton.Left := 1000;
        if Assigned(TAdvQuickAccessToolBar(ATB).Images) then
          ATBButton.ImageIndex := ATBButton.Index;
        ATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
      end;
    end;
  1:
    begin
      ATBMenuButton := TAdvToolBarMenuButton(Designer.CreateComponent(TAdvToolBarMenuButton,Component,0, 0,45,23));
      ATBMenuButton.Width := 23;
      ATBMenuButton.Height := 22;
      ATBMenuButton.Parent := ATB;
      ATBMenuButton.Caption := ''; //ATBMenuButton.Name;
      if (TAdvCustomGlowButton(Component).Parent is TAdvToolBar) then
        ATBMenuButton.AdvToolBar := TAdvToolBar(ATB)
      else if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATBMenuButton.Width := 22;
        ATBMenuButton.Height := 20;
        ATBMenuButton.Left := 1000;
        ATBMenuButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
      end;
    end;
  2:
    begin
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
        Exit;

      ATBSeparator := TAdvToolBarSeparator(Designer.CreateComponent(TAdvToolBarSeparator,Component,0, 0,10,23));
      ATBSeparator.Parent := ATB;
      if (TAdvGlowButton(Component).Parent is TAdvToolBar) then
        ATBSeparator.AdvToolBar := TAdvToolBar(ATB);
    end;
  3:
    begin
      {$IFNDEF TMS_STD}
      DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
      DBATBButton.Width := 23;
      DBATBButton.Height := 22;
      DBATBButton.Parent := ATB;
      DBATBButton.Caption := '';
      if (TAdvCustomGlowButton(Component).Parent is TAdvToolBar) then
        DBATBButton.AdvToolBar := TAdvToolBar(ATB)
      else if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        DBATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
        DBATBButton.Left := 1000;
      end;
      {$ENDIF}
    end;
  4:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        DBATBButton := TDBAdvToolBarButton(Designer.CreateComponent(TDBAdvToolBarButton,Component,0, 0,45,23));
        DBATBButton.Width := 23;
        DBATBButton.Height := 22;
        DBATBButton.Parent := ATB;
        DBATBButton.Caption := '';
        if (TAdvCustomGlowButton(Component).Parent is TAdvToolBar) then
          DBATBButton.AdvToolBar := TAdvToolBar(ATB)
        else if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
        begin
          DBATBButton.AdvQuickAccessToolBar := TAdvQuickAccessToolBar(ATB);
          DBATBButton.Left := 1000;
        end;

        case I of
          1:
          begin
            DBATBButton.DBButtonType := dbtFirst;
            DBATBButton.Hint := SFirstRecord;
          end;
          2:
          begin
            DBATBButton.DBButtonType := dbtPrior;
            DBATBButton.Hint := SPriorRecord;
          end;
          3:
          begin
            DBATBButton.DBButtonType := dbtNext;
            DBATBButton.Hint := SNextRecord;
          end;
          4:
          begin
            DBATBButton.DBButtonType := dbtLast;
            DBATBButton.Hint := SLastRecord;
          end;
          5:
          begin
            DBATBButton.DBButtonType := dbtInsert;
            DBATBButton.Hint := SInsertRecord;
          end;
          6:
          begin
            DBATBButton.ConfirmAction := True;
            DBATBButton.DBButtonType := dbtDelete;
            DBATBButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            DBATBButton.DBButtonType := dbtEdit;
            DBATBButton.Hint := SEditRecord;
          end;
          8:
          begin
            DBATBButton.DBButtonType := dbtPost;
            DBATBButton.Hint := SPostEdit;
          end;
          9:
          begin
            DBATBButton.DBButtonType := dbtCancel;
            DBATBButton.Hint := SCancelEdit;
          end;
          10:
          begin
            DBATBButton.DBButtonType := dbtRefresh;
            DBATBButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}
    end;
  5:
    begin
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
        Exit;

      ATBContainer := TAdvToolBarContainer(Designer.CreateComponent(TAdvToolBarContainer,Component,0, 0,65,45));
      ATBContainer.Width := 65;
      ATBContainer.Height := 45;
      ATBContainer.Parent := ATB;
      //ATBContainer.Caption := '';
      if (TAdvCustomGlowButton(Component).Parent is TAdvToolBar) then
        ATBContainer.AdvToolBar := TAdvToolBar(ATB);
    end;
  6:
    begin
      AGlowButton := TAdvGlowButton(Designer.CreateComponent(TAdvGlowButton,Component,0, 0,23,22));
      AGlowButton.Width := 23;
      AGlowButton.Height := 22;
      AGlowButton.Parent := ATB;
      AGlowButton.Caption := '';

      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AGlowButton.Width := 22;
        AGlowButton.Height := 20;
        AGlowButton.Transparent := True;
        AGlowButton.Left := 1000;
      end;
    end;
  7:
    begin
      AGlowMenuButton := TAdvGlowMenuButton(Designer.CreateComponent(TAdvGlowMenuButton,Component,0, 0,23,22));
      AGlowMenuButton.Width := 23;
      AGlowMenuButton.Height := 22;
      AGlowMenuButton.Parent := ATB;
      AGlowMenuButton.Caption := '';
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AGlowMenuButton.Left := 1000;
      end;
    end;
  8:
    begin
      {$IFNDEF TMS_STD}
      ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
      ADBGlowButton.Width := 23;
      ADBGlowButton.Height := 22;
      ADBGlowButton.Parent := ATB;
      ADBGlowButton.Caption := '';
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ADBGlowButton.Left := 1000;
      end;
      {$ENDIF}
    end;
  9:
    begin
      {$IFNDEF TMS_STD}
      for I := 1 to 10 do
      begin
        ADBGlowButton := TDBAdvGlowButton(Designer.CreateComponent(TDBAdvGlowButton,Component,0, 0,23,22));
        ADBGlowButton.Width := 23;
        ADBGlowButton.Height := 22;
        ADBGlowButton.Parent := ATB;
        ADBGlowButton.Caption := '';
        if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
        begin
          ADBGlowButton.Left := 1000;
        end;

        case I of
          1:
          begin
            ADBGlowButton.DBButtonType := dbFirst;
            ADBGlowButton.Hint := SFirstRecord;
          end;
          2:
          begin
            ADBGlowButton.DBButtonType := dbPrior;
            ADBGlowButton.Hint := SPriorRecord;
          end;
          3:
          begin
            ADBGlowButton.DBButtonType := dbNext;
            ADBGlowButton.Hint := SNextRecord;
          end;
          4:
          begin
            ADBGlowButton.DBButtonType := dbLast;
            ADBGlowButton.Hint := SLastRecord;
          end;
          5:
          begin
            ADBGlowButton.DBButtonType := dbInsert;
            ADBGlowButton.Hint := SInsertRecord;
          end;
          6:
          begin
            ADBGlowButton.ConfirmAction := True;
            ADBGlowButton.DBButtonType := dbDelete;
            ADBGlowButton.Hint := SDeleteRecord;
          end;
          7:
          begin
            ADBGlowButton.DBButtonType := dbEdit;
            ADBGlowButton.Hint := SEditRecord;
          end;
          8:
          begin
            ADBGlowButton.DBButtonType := dbPost;
            ADBGlowButton.Hint := SPostEdit;
          end;
          9:
          begin
            ADBGlowButton.DBButtonType := dbCancel;
            ADBGlowButton.Hint := SCancelEdit;
          end;
          10:
          begin
            ADBGlowButton.DBButtonType := dbRefresh;
            ADBGlowButton.Hint := SRefreshRecord;
          end;
        end;
      end;
      {$ENDIF}
    end;
  {$IFDEF TMSPACK}
  10:
    begin
      ACOLSelector := TAdvOfficeColorSelector(Designer.CreateComponent(TAdvOfficeColorSelector,Component,0, 0,23,22));
      ACOLSelector.Width := 23;
      ACOLSelector.Height := 22;
      ACOLSelector.Parent := ATB;
      ACOLSelector.Caption := '';
      AColSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ACOLSelector.Width := 22;
        ACOLSelector.Height := 20;
        ACOLSelector.Left := 1000;
      end;
    end;
  11:
    begin
      ATCOLSelector := TAdvOfficeTextColorSelector(Designer.CreateComponent(TAdvOfficeTextColorSelector,Component,0, 0,23,22));
      ATCOLSelector.Width := 23;
      ATCOLSelector.Height := 22;
      ATCOLSelector.Parent := ATB;
      ATCOLSelector.Caption := '';
      ATCOLSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATCOLSelector.Width := 22;
        ATCOLSelector.Height := 20;
        ATCOLSelector.Left := 1000;
      end;
    end;
  12:
    begin
      APENSSelector := TAdvOfficePenStyleSelector(Designer.CreateComponent(TAdvOfficePenStyleSelector,Component,0, 0,23,22));
      APENSSelector.Width := 23;
      APENSSelector.Height := 22;
      APENSSelector.Parent := ATB;
      APENSSelector.Caption := '';
      APENSSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        APENSSelector.Width := 22;
        APENSSelector.Height := 20;
        APENSSelector.Left := 1000;
      end;
    end;
  13:
    begin
      APENWSelector := TAdvOfficePenWidthSelector(Designer.CreateComponent(TAdvOfficePenWidthSelector,Component,0, 0,23,22));
      APENWSelector.Width := 23;
      APENWSelector.Height := 22;
      APENWSelector.Parent := ATB;
      APENWSelector.Caption := '';
      APENWSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        APENWSelector.Width := 22;
        APENWSelector.Height := 20;
        APENWSelector.Left := 1000;
      end;
    end;
  14:
    begin
      ABRUSHSelector := TAdvOfficeBrushStyleSelector(Designer.CreateComponent(TAdvOfficeBrushStyleSelector,Component,0, 0,23,22));
      ABRUSHSelector.Width := 23;
      ABRUSHSelector.Height := 22;
      ABRUSHSelector.Parent := ATB;
      ABRUSHSelector.Caption := '';
      ABRUSHSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ABRUSHSelector.Width := 22;
        ABRUSHSelector.Height := 20;
        ABRUSHSelector.Left := 1000;
      end;
    end;
  15:
    begin
      AGRADSelector := TAdvOfficeGradientDirectionSelector(Designer.CreateComponent(TAdvOfficeGradientDirectionSelector,Component,0, 0,23,22));
      AGRADSelector.Width := 23;
      AGRADSelector.Height := 22;
      AGRADSelector.Parent := ATB;
      AGRADSelector.Caption := '';
      AGRADSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AGRADSelector.Width := 22;
        AGRADSelector.Height := 20;
        AGRADSelector.Left := 1000;
      end;
    end;
  16:
    begin
      ASHADSelector := TAdvOfficeShadowSelector(Designer.CreateComponent(TAdvOfficeShadowSelector,Component,0, 0,23,22));
      ASHADSelector.Width := 23;
      ASHADSelector.Height := 22;
      ASHADSelector.Parent := ATB;
      ASHADSelector.Caption := '';
      ASHADSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ASHADSelector.Width := 22;
        ASHADSelector.Height := 20;
        ASHADSelector.Left := 1000;
      end;
    end;
  17:
    begin
      ATABLESelector := TAdvOfficeTableSelector(Designer.CreateComponent(TAdvOfficeTableSelector,Component,0, 0,23,22));
      ATABLESelector.Width := 23;
      ATABLESelector.Height := 22;
      ATABLESelector.Parent := ATB;
      ATABLESelector.Caption := '';
      ATABLESelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATABLESelector.Width := 22;
        ATABLESelector.Height := 20;
        ATABLESelector.Left := 1000;
      end;
    end;
  18:
    begin
      ATBORDERSelector := TAdvOfficeTableBorderSelector(Designer.CreateComponent(TAdvOfficeTableBorderSelector,Component,0, 0,23,22));
      ATBORDERSelector.Width := 23;
      ATBORDERSelector.Height := 22;
      ATBORDERSelector.Parent := ATB;
      ATBORDERSelector.Caption := '';
      ATBORDERSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATBORDERSelector.Width := 22;
        ATBORDERSelector.Height := 20;
        ATBORDERSelector.Left := 1000;
      end;
    end;
  19:
    begin
      ACHARSelector := TAdvOfficeCharacterSelector(Designer.CreateComponent(TAdvOfficeCharacterSelector,Component,0, 0,23,22));
      ACHARSelector.Width := 23;
      ACHARSelector.Height := 22;
      ACHARSelector.Parent := ATB;
      ACHARSelector.Caption := '';
      ACHARSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ACHARSelector.Width := 22;
        ACHARSelector.Height := 20;
        ACHARSelector.Left := 1000;
      end;
    end;
  20:
    begin
      ATOOLSelector := TAdvOfficeToolSelector(Designer.CreateComponent(TAdvOfficeToolSelector,Component,0, 0,23,22));
      ATOOLSelector.Width := 23;
      ATOOLSelector.Height := 22;
      ATOOLSelector.Parent := ATB;
      ATOOLSelector.Caption := '';
      ATOOLSelector.FocusType := ftHot;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        ATOOLSelector.Width := 22;
        ATOOLSelector.Height := 20;
        ATOOLSelector.Left := 1000;
      end;
    end;
  21:
    begin
      AFONTSelector := TAdvOfficeFontSelector(Designer.CreateComponent(TAdvOfficeFontSelector,Component,0, 0,100,22));
      AFONTSelector.Width := 100;
      AFONTSelector.Height := 22;
      AFONTSelector.Parent := ATB;
      //AFONTSelector.Caption := '';
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AFONTSelector.Width := 22;
        AFONTSelector.Height := 20;
        AFONTSelector.Left := 1000;
      end;
    end;
  22:
    begin
      AFSIZESelector := TAdvOfficeFontSizeSelector(Designer.CreateComponent(TAdvOfficeFontSizeSelector,Component,0, 0,40,22));
      AFSIZESelector.Width := 40;
      AFSIZESelector.Height := 22;
      AFSIZESelector.Parent := ATB;
      //AFONTSelector.Caption := '';
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AFSIZESelector.Width := 22;
        AFSIZESelector.Height := 20;
        AFSIZESelector.Left := 1000;
      end;
    end;
  23:
    begin
      AFSCROLLSelector := TAdvOfficeScrollSelector(Designer.CreateComponent(TAdvOfficeScrollSelector,Component,0, 0,240,60));
      AFSCROLLSelector.Width := 240;
      AFSCROLLSelector.Height := 60;
      AFSCROLLSelector.Parent := ATB;
      if (TAdvCustomGlowButton(Component).Parent is TAdvQuickAccessToolBar) then
      begin
        AFSCROLLSelector.Width := 22;
        AFSCROLLSelector.Height := 20;
        AFSCROLLSelector.Left := 1000;
      end;
    end;
  {$ENDIF}
  end;
end;

function TAdvGlowButtonEditor.GetVerb(Index: Integer): string;
begin
  if (Component is TAdvCustomGlowButton) then
  begin
    if ((Component as TAdvCustomGlowButton).Parent is TAdvQuickAccessToolBar) then
    begin
      case Index of
      0: Result := 'Add Button';
      1: Result := 'Add MenuButton';
      2: Result := 'Add AdvGlowButton';
      3: Result := 'Add AdvGlowMenuButton';
      end;
      Exit;
    end
    else if not ((Component as TAdvCustomGlowButton).Parent is TAdvToolBar) then
    begin
      case Index of
      0: Result := 'Silver look';
      1: Result := 'Blue look';
      2: Result := 'Black look';
      end;
      Exit;
    end;
  end;

  case Index of
  0: Result := 'Add Button';
  1: Result := 'Add MenuButton';
  2: Result := 'Add Separator';
  3: Result := 'Add DBButton';
  4: Result := 'Add DBNavigator';
  5: Result := 'Add Container';
  6: Result := 'Add AdvGlowButton';
  7: Result := 'Add AdvGlowMenuButton';
  8: Result := 'Add DBAdvGlowButton';
  9: Result := 'Add DBAdvGlowNavigator';
  {$IFDEF TMSPACK}
  10: Result := 'Add AdvOfficeColorSelector';
  11: Result := 'Add AdvOfficeTextColorSelector';
  12: Result := 'Add AdvOfficePenStyleSelector';
  13: Result := 'Add AdvOfficePenWidthSelector';
  14: Result := 'Add AdvOfficeBrushStyleSelector';
  15: Result := 'Add AdvOfficeGradientDirectionSelector';
  16: Result := 'Add AdvOfficeShadowSelector';
  17: Result := 'Add AdvOfficeTableSelector';
  18: Result := 'Add AdvOfficeTableBorderSelector';
  19: Result := 'Add AdvOfficeCharacterSelector';
  20: Result := 'Add AdvOfficeToolSelector';
  21: Result := 'Add AdvOfficeFontSelector';
  22: Result := 'Add AdvOfficeFontSizeSelector';
  23: Result := 'Add AdvOfficeScrollSelector';
  {$ENDIF}
  end;
end;

function TAdvGlowButtonEditor.GetVerbCount: Integer;
begin
  if (Component is TAdvCustomGlowButton) then
  begin
    if ((Component as TAdvCustomGlowButton).Parent is TAdvQuickAccessToolBar) then
    begin
      Result := 4;
      Exit;
    end
    else if not ((Component as TAdvCustomGlowButton).Parent is TAdvToolBar) then
    begin
      Result := 3;
      Exit;
    end;
  end;
  {$IFDEF TMSPACK}
  Result := 24;
  {$ELSE}
  Result := 10;
  {$ENDIF}
end;

{ TAdvToolBarPagerEditor }

procedure TAdvToolBarPagerEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvPage;
  QAT: TAdvQuickAccessToolBar;
  ASB: TAdvShapeButton;
  i: integer;
  hasappmenu: boolean;
begin
  inherited;
  case Index of
  0:
    begin
      //TAdvToolBarPager(Component).ControlStyle := TAdvToolBarPager(Component).ControlStyle + [csAcceptsControls];
      AdvPage := TAdvPage(Designer.CreateComponent(TAdvPage,Component,23,0,100,100));
      AdvPage.Parent := TAdvToolBarPager(Component);
      AdvPage.AdvToolBarPager := TAdvToolBarPager(Component);
      AdvPage.Caption := AdvPage.name;
      TAdvToolBarpager(component).ActivePage:= AdvPage;
      //TAdvToolBarPager(Component).Update;
      //TAdvToolBarPager(Component).Invalidate;
      //TAdvToolBarPager(Component).ControlStyle := TAdvToolBarPager(Component).ControlStyle - [csAcceptsControls];
    end;
  1: TAdvToolBarPager(Component).SelectNextPage(false);
  2: TAdvToolBarPager(Component).SelectNextPage(True);
  3:
    begin
      if not Assigned(TAdvToolBarPager(Component).QuickAccessToolBar) then
      begin
        QAT := TAdvQuickAccessToolBar(Designer.CreateComponent(TAdvQuickAccessToolBar,Component, 60,0,100,25));
        QAT.Parent := TAdvToolBarPager(Component);
        QAT.Left := 10;
      end;
      //QAT.AdvToolBarPager := TAdvToolBarPager(Component);
    end;
  4:begin
      hasappmenu := false;
      for i := 0 to TAdvToolBarPager(Component).ControlCount - 1 do
      begin
        if (TAdvToolBarPager(Component).Controls[i] is TAdvShapeButton) then
          hasappmenu := true;
      end;

      if not hasappmenu then
      begin
        ASB := TAdvShapeButton(Designer.CreateComponent(TAdvShapeButton,Component, 0,30,55,24));
        ASB.Parent := TAdvToolBarPager(Component);
        ASB.InitializeStyle;
      end
      else
        ShowMessage('TAdvToolBarPager already has an Application Menu Button');
    end;
  5:begin
      ShowMessage('TMS Advanced ToolBars && Menus version ' + TAdvToolBarPager(Component).Version+#13#10'Copyright © 2005 - 2010 by TMS software');
    end;
  end;
end;

function TAdvToolBarPagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  3: Result := 'Add Quick Access Toolbar';
  4: Result := 'Add Application Menu Button';
  5: Result := 'About';
  end;
end;

function TAdvToolBarPagerEditor.GetVerbCount: Integer;
begin
  Result := 6;
end;

{ TAdvPageEditor }

procedure TAdvPageEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvPage;
  AToolBar: TAdvToolBar;
begin
  inherited;
  case Index of
  0:
    begin
      //TWinControl(Component).Parent.ControlStyle := TWinControl(Component).Parent.ControlStyle + [csAcceptsControls];
      AdvPage := TAdvPage(Designer.CreateComponent(TAdvPage,TWinControl(Component).Parent,23,0,100,100));
      AdvPage.Parent := TWinControl(Component).Parent;
      AdvPage.AdvToolBarPager := TAdvToolBarPager(TWinControl(Component).Parent);
      AdvPage.Caption := AdvPage.Name;
      TAdvToolBarPager(TWinControl(Component).Parent).ActivePage:= AdvPage;

      //TAdvToolBarPager(TWinControl(Component).Parent).Update;
      //(TWinControl(Component).Parent as TWinControl).Invalidate;
      //TWinControl(Component).Parent.ControlStyle := TWinControl(Component).Parent.ControlStyle - [csAcceptsControls];
    end;
  1: TAdvToolBarPager(TAdvPage(Component).Parent).SelectNextPage(false);
  2: TAdvToolBarPager(TAdvPage(Component).Parent).SelectNextPage(true);
  3:
    begin
    TAdvPage(Component).AdvToolBarPager := nil;
    Component.Free;
    end;
  4:
    begin
      AdvPage := TAdvPage(Component);
      if True{TAdvToolBarPager(TAdvPage(Component).Parent).Align in [daTop, daBottom]} then
      begin
        AToolBar := TAdvToolBar(Designer.CreateComponent(TAdvToolBar,Component,0, 0,45,23));
        AToolBar.AutoSize := False;
        AToolBar.ShowRightHandle := False;
        AToolBar.ShowCaption := True;
        AToolBar.CaptionPosition := cpBottom;
        AToolBar.CaptionAlignment := taCenter;
        AToolBar.Width := 120;
        AToolBar.Height := 26;
        AToolBar.ParentStyler := True;
      end
      else
      begin
        AToolBar := TAdvToolBar(Designer.CreateComponent(TAdvToolBar,Component,0, 0,23,45));
        AToolBar.Width := 26;
        AToolBar.Height := 120;
      end;
      AToolBar.Parent := AdvPage;
      AToolBar.Caption := 'Untitled';
    end;
  end;
end;

function TAdvPageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  3: Result := 'Delete Page';
  4: Result := 'Add ToolBar';
  end;
end;

function TAdvPageEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


{ TGDIPATBPictureProperty }

procedure TGDIPATBPictureProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
begin
  inherited;
  OpenDialog := TOpenPictureDialog.Create(nil);

  OpenDialog.Filter := 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.png)|*.jpg;*.jpeg;*.gif;*.bmp;*.png|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|PNG files (*.png)|*.png';

  if Opendialog.Execute then
  begin
    TGDIPPicture(GetOrdValue).LoadFromFile(Opendialog.FileName);
    Modified;
  end;
  OpenDialog.Free;
end;

function TGDIPATBPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TGDIPATBPictureProperty.GetValue: String;
begin
  if not TGDIPPicture(GetOrdValue).Empty then
    Result := '(TPicture)'
  else
    Result := '(None)';
end;

procedure TGDIPATBPictureProperty.SetValue(const Value: String);
var
  gdip: TGDIPPicture;
begin
  inherited;

  if (Value = '') then  // picture is cleared
  begin
    gdip := TGDIPPicture(GetOrdValue);
    if Assigned(gdip) then
      gdip.Assign(nil);
  end;
end;


{ TAdvQuickAccessToolBarEditor }

procedure TAdvQuickAccessToolBarEditor.ExecuteVerb(Index: Integer);
var
  ATBButton : TAdvToolBarButton;
  ATBMenuButton: TAdvToolBarMenuButton;
  QATB: TAdvQuickAccessToolBar;
  AGlowButton: TAdvGlowButton;
  AGlowMenuButton: TAdvGlowMenuButton;
  ATBSeparator: TAdvToolBarSeparator;
begin
  inherited;

  QATB := TAdvQuickAccessToolBar(Component);

  case Index of
  0:
    begin
      ATBButton := TAdvToolBarButton(Designer.CreateComponent(TAdvToolBarButton,Component,0, 0,22,20));
      ATBButton.Width := 22;
      ATBButton.Height := 20;
      ATBButton.Parent := QATB;
      ATBButton.Caption := '';
      ATBButton.Left := 1000;
      if Assigned(QATB.Images) then
      begin
        if ATBButton.Index < QATB.Images.Count then
          ATBButton.ImageIndex := ATBButton.Index;
      end;
      ATBButton.AdvQuickAccessToolBar := QATB;
    end;
  1:
    begin
      ATBMenuButton := TAdvToolBarMenuButton(Designer.CreateComponent(TAdvToolBarMenuButton,Component,0, 0,22,20));
      ATBMenuButton.Width := 22;
      ATBMenuButton.Height := 20;
      ATBMenuButton.Parent := QATB;
      ATBMenuButton.Caption := '';
      ATBMenuButton.Left := 1000;
      ATBMenuButton.DropDownButton := true;
      if Assigned(QATB.Images) then
      begin
        if ATBMenuButton.Index < QATB.Images.Count then
          ATBMenuButton.ImageIndex := ATBMenuButton.Index;
      end;
      ATBMenuButton.AdvQuickAccessToolBar := QATB;
    end;

  2:
    begin
      AGlowButton := TAdvGlowButton(Designer.CreateComponent(TAdvGlowButton,Component,0, 0,22,20));
      AGlowButton.Width := 22;
      AGlowButton.Height := 20;
      AGlowButton.Parent := QATB;
      AGlowButton.FocusType := ftHot;
      AGlowButton.Left := 1000;
      AGlowButton.Transparent := True;
      if Assigned(QATB.Images) then
        AGlowButton.Images := TImageList(QATB.Images);
      if Assigned(QATB.DisabledImages) then
        AGlowButton.DisabledImages := TImageList(QATB.DisabledImages);
      AGlowButton.Caption := '';
      if (QATB.Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(QATB.Parent).ToolBarStyler) then
        AGlowButton.Appearance.Assign(TAdvToolBarPager(QATB.Parent).ToolBarStyler.GlowButtonAppearance);
    end;
  3:
    begin
      AGlowMenuButton := TAdvGlowMenuButton(Designer.CreateComponent(TAdvGlowMenuButton,Component,0, 0,22,20));
      AGlowMenuButton.Width := 22;
      AGlowMenuButton.Height := 20;
      AGlowMenuButton.Parent := QATB;
      AGlowMenuButton.FocusType := ftHot;
      AGlowMenuButton.Left := 1000;
      AGlowMenuButton.Transparent := True;
      if Assigned(QATB.Images) then
        AGlowMenuButton.Images := TImageList(QATB.Images);
      if Assigned(QATB.DisabledImages) then
        AGlowMenuButton.DisabledImages := TImageList(QATB.DisabledImages);
      AGlowMenuButton.Caption := '';
      if (QATB.Parent is TAdvToolBarPager) and Assigned(TAdvToolBarPager(QATB.Parent).ToolBarStyler) then
        AGlowMenuButton.Appearance.Assign(TAdvToolBarPager(QATB.Parent).ToolBarStyler.GlowButtonAppearance);
    end;

  4:
    begin
      ATBSeparator := TAdvToolBarSeparator(Designer.CreateComponent(TAdvToolBarSeparator,Component,0, 0,10,23));
      ATBSeparator.Parent := QATB;
  //    ATBSeparator.AdvToolBar := QATB;
    end;

  end;
end;

function TAdvQuickAccessToolBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Add Button';
  1: Result := 'Add MenuButton';
  2: Result := 'Add AdvGlowButton';
  3: Result := 'Add AdvGlowMenuButton';
  4: Result := 'Add Separator';
  end;
end;

function TAdvQuickAccessToolBarEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

end.
