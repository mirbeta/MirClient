{*************************************************************************}
{ TMS TAdvToolBar Popup                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
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

unit AdvToolBarPopup;

interface

uses
  Classes, AdvToolbar, Controls, Windows, Messages, Graphics,
  AdvGlowButton, AdvOfficeComboBox, AdvOfficeSelectors;


type
  TAdvPopupToolBarOption = (oBold,oItalic,oUnderline,oStrikeThrough,oSubscript,oSuperScript,
    oColor,oFontColor,oFont,oFontSize,oAlignLeft,oAlignCenter,oAlignRight);

  TAdvPopupToolBarOptions = set of TAdvPopupToolBarOption;

  TFontStyleType = (fstBold, fstItalic, fstUnderline, fstStrikeThrough, fstSubScript, fstSuperScript);

  TAdvPopupToolBarWindow = class(THintWindow)
  private
    FToolBar: TAdvToolBar;
    FBoldButton: TAdvGlowButton;
    FItalicButton: TAdvGlowButton;
    FUnderlineButton: TAdvGlowButton;
    FStrikethroughButton: TAdvGlowButton;
    FSubScriptButton: TAdvGlowButton;
    FSuperScriptButton: TAdvGlowButton;
    FAlignLeftButton: TAdvGlowButton;
    FAlignCenterButton: TAdvGlowButton;
    FAlignRightButton: TAdvGlowButton;
    FOptions: TAdvPopupToolBarOptions;
    FFontSelector: TAdvOfficeFontSelector;
    FFontSizeSelector: TAdvOfficeFontSizeSelector;
    FTxtColorSelector: TAdvOfficeColorSelector;
    FBkColorSelector: TAdvOfficeColorSelector;
    FToolBarStyler: TCustomAdvToolBarStyler;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure ButtonClick(Sender: TObject);
    procedure AlignClick(Sender: TObject);
    procedure ColorSelect(Sender: TObject; AColor: TColor);
    procedure FontSelect(Sender: TObject; AName: string);
    procedure FontSizeSelect(Sender: TObject; ASize: integer);
    procedure SetToolBarStyler(const Value: TCustomAdvToolBarStyler);
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    function DoFontStyle(AType: TFontStyleType): boolean; virtual;
    procedure DoFontSize(ASize: integer); virtual;
    procedure DoFont(AName: string); virtual;
    procedure DoFontColor(AColor: TColor); virtual;
    procedure DoColor(AColor: TColor); virtual;
    function DoAlign(AAlign: TAlignment): boolean; virtual;
    procedure Init;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ToolBarStyler: TCustomAdvToolBarStyler read FToolBarStyler write SetToolBarStyler;
    property Options: TAdvPopupToolBarOptions read FOptions write FOptions;
    procedure SetFontStyleState(AType: TFontStyleType; State: boolean);
    procedure SetFontColor(AColor: TColor);
    procedure SetColor(AColor: TColor);
    procedure SetFont(AName: string);
    procedure SetFontSize(ASize: integer);
  end;


implementation

uses
  Math;

type
  TAdvToolBarEx = class(TAdvToolBar);


{ TAdvPopupToolBarWindow }

procedure TAdvPopupToolBarWindow.AlignClick(Sender: TObject);
begin
  DoAlign(TAlignment((Sender as TAdvGlowButton).Tag));

  FAlignLeftButton.Down := (Sender as TAdvGlowButton).Tag = integer(taLeftJustify);
  FAlignRightButton.Down := (Sender as TAdvGlowButton).Tag = integer(taRightJustify);
  FAlignCenterButton.Down := (Sender as TAdvGlowButton).Tag = integer(taCenter);
end;

procedure TAdvPopupToolBarWindow.ButtonClick(Sender: TObject);
begin
  (Sender as TAdvGlowButton).Down := DoFontStyle(TFontStyleType((Sender as TAdvGlowButton).Tag));
end;

procedure TAdvPopupToolBarWindow.ColorSelect(Sender: TObject; AColor: TColor);
begin
  case (Sender as TAdvOfficeColorSelector).Tag of
  0: DoFontColor(AColor);
  1: DoColor(AColor);
  end;
end;

constructor TAdvPopupToolBarWindow.Create(AOwner: TComponent);
begin
  inherited;
  FToolBar := TAdvToolBar.Create(Self);

  FBoldButton := TAdvGlowButton.Create(Self);
  FBoldButton.Tag := 0;
  FBoldButton.Style := bsCheck;
  FBoldButton.Picture.LoadFromResourceName(HInstance, 'TMSRETBBOLD');
  FBoldButton.OnClick := ButtonClick;

  FItalicButton := TAdvGlowButton.Create(Self);
  FItalicButton.Tag := 1;
  FItalicButton.Style := bsCheck;
  FItalicButton.Picture.LoadFromResourceName(HInstance,'TMSRETBITALIC');
  FItalicButton.OnClick := ButtonClick;

  FUnderlineButton := TAdvGlowButton.Create(Self);
  FUnderlineButton.Tag := 2;
  FUnderlineButton.Style := bsCheck;
  FUnderlineButton.Picture.LoadFromResourceName(HInstance,'TMSRETBUNDERLINE');
  FUnderlineButton.OnClick := ButtonClick;

  FStrikethroughButton := TAdvGlowButton.Create(Self);
  FStrikethroughButton.Tag := 3;
  FStrikethroughButton.Style := bsCheck;
  FStrikethroughButton.Picture.LoadFromResourceName(HInstance,'TMSRETBSTRIKE');
  FStrikethroughButton.OnClick := ButtonClick;

  FSubScriptButton := TAdvGlowButton.Create(Self);
  FSubScriptButton.Tag := 4;
  FSubScriptButton.Style := bsCheck;
  FSubScriptButton.Picture.LoadFromResourceName(HInstance,'TMSRETBSUBSCR');
  FSubScriptButton.OnClick := ButtonClick;

  FSuperScriptButton := TAdvGlowButton.Create(Self);
  FSuperScriptButton.Tag := 5;
  FSuperScriptButton.Style := bsCheck;
  FSuperScriptButton.Picture.LoadFromResourceName(HInstance,'TMSRETBSUPERSCR');
  FSuperScriptButton.OnClick := ButtonClick;

  FAlignLeftButton := TAdvGlowButton.Create(Self);
  FAlignLeftButton.Tag := integer(taLeftJustify);
  FAlignLeftButton.Style := bsCheck;
  FAlignLeftButton.Picture.LoadFromResourceName(HInstance,'TMSRETBALIGNLEFT');
  FAlignLeftButton.OnClick := AlignClick;

  FAlignCenterButton := TAdvGlowButton.Create(Self);
  FAlignCenterButton.Tag := integer(taCenter);
  FAlignCenterButton.Style := bsCheck;
  FAlignCenterButton.Picture.LoadFromResourceName(HInstance,'TMSRETBALIGNCENTER');
  FAlignCenterButton.OnClick := AlignClick;

  FAlignRightButton := TAdvGlowButton.Create(Self);
  FAlignRightButton.Tag := integer(taRightJustify);
  FAlignRightButton.Style := bsCheck;
  FAlignRightButton.Picture.LoadFromResourceName(HInstance,'TMSRETBALIGNRIGHT');
  FAlignRightButton.OnClick := AlignClick;

  FTxtColorSelector := TAdvOfficeColorSelector.Create(Self);
  FTxtColorSelector.Tag := 0;
  FTxtColorSelector.OnSelectColor := ColorSelect;

  FBkColorSelector := TAdvOfficeColorSelector.Create(Self);
  FBkColorSelector.Tag := 1;
  FBkColorSelector.OnSelectColor := ColorSelect;

  FFontSizeSelector := TAdvOfficeFontSizeSelector.Create(Self);
  FFontSizeSelector.OnSelectFontSize := FontSizeSelect;

  FFontSelector := TAdvOfficeFontSelector.Create(Self);
  FFontSelector.OnSelectFontName := FontSelect;

  Init;
end;

procedure TAdvPopupToolBarWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style AND NOT WS_BORDER;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

procedure TAdvPopupToolBarWindow.CreateWnd;
begin
  inherited;
end;

destructor TAdvPopupToolBarWindow.Destroy;
begin
  FBoldButton.Free;
  FItalicButton.Free;
  FUnderlineButton.Free;
  FStrikeThroughButton.Free;
  FSuperScriptButton.Free;
  FSubScriptButton.Free;

  FTxtColorSelector.Free;
  FBkColorSelector.Free;

  FFontSizeSelector.Free;
  FFontSelector.Free;

  inherited;
end;

function TAdvPopupToolBarWindow.DoAlign(AAlign: TAlignment): boolean;
begin
  //
  Result := false;
end;

procedure TAdvPopupToolBarWindow.DoColor(AColor: TColor);
begin
  //
end;

procedure TAdvPopupToolBarWindow.DoFont(AName: string);
begin
  //
end;

procedure TAdvPopupToolBarWindow.DoFontColor(AColor: TColor);
begin
  //
end;

procedure TAdvPopupToolBarWindow.DoFontSize(ASize: integer);
begin
  //
end;

function TAdvPopupToolBarWindow.DoFontStyle(AType: TFontStyleType): boolean;
begin
  //
  Result := false;
end;

procedure TAdvPopupToolBarWindow.FontSelect(Sender: TObject; AName: string);
begin
  DoFont(AName);
end;

procedure TAdvPopupToolBarWindow.FontSizeSelect(Sender: TObject;
  ASize: integer);
begin
  DoFontSize(ASize);
end;

procedure TAdvPopupToolBarWindow.Init;
var
  btnx: integer;
begin
  FToolBar.Parent := Self;
  FToolBar.ShowRightHandle := false;
  FToolBar.AutoArrangeButtons := false;
  FToolBar.AutoSize := false;
  FToolBar.AutoPositionControls := false;
  FToolBar.ShowClose := false;
  FToolBar.ToolBarStyler := ToolBarStyler;
  FToolBar.Visible  := true;
  FToolBar.Left := 0;
  FToolBar.Top := 0;
  FToolBar.Width := 160;
  FToolBar.Height := 44;

  btnx := 0;

  if oBold in Options then
  begin
    FBoldButton.Parent := FToolBar;
    FBoldButton.Left := btnx;
    FBoldButton.Top := 22;
    FBoldButton.Width := 20;
    FBoldButton.Height := 20;
    FBoldButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oItalic in Options then
  begin
    FItalicButton.Parent := FToolBar;
    FItalicButton.Left := btnx;
    FItalicButton.Top := 22;
    FItalicButton.Width := 20;
    FItalicButton.Height := 20;
    FItalicButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oUnderline in Options then
  begin
    FUnderlineButton.Parent := FToolBar;
    FUnderlineButton.Left := btnx;
    FUnderlineButton.Top := 22;
    FUnderlineButton.Width := 20;
    FUnderlineButton.Height := 20;
    FUnderlineButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oStrikeThrough in Options then
  begin
    FStrikeThroughButton.Parent := FToolBar;
    FStrikeThroughButton.Left := btnx;
    FStrikeThroughButton.Top := 22;
    FStrikeThroughButton.Width := 20;
    FStrikeThroughButton.Height := 20;
    FStrikeThroughButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oSubscript in Options then
  begin
    FSubScriptButton.Parent := FToolBar;
    FSubScriptButton.Left := btnx;
    FSubScriptButton.Top := 22;
    FSubScriptButton.Width := 20;
    FSubScriptButton.Height := 20;
    FSubScriptButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oSuperScript in Options then
  begin
    FSuperScriptButton.Parent := FToolBar;
    FSuperScriptButton.Left := btnx;
    FSuperScriptButton.Top := 22;
    FSuperScriptButton.Width := 20;
    FSuperScriptButton.Height := 20;
    FSuperScriptButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oFontColor in Options then
  begin
    FTxtColorSelector.Parent := FToolBar;
    FTxtColorSelector.Left := btnx;
    FTxtColorSelector.Top := 22;
    FTxtColorSelector.Width := 20;
    FTxtColorSelector.Height := 20;
    FTxtColorSelector.Visible := true;
    btnx := btnx + 20;
  end;

  if oColor in Options then
  begin
    FBkColorSelector.Parent := FToolBar;
    FBkColorSelector.Left := btnx;
    FBkColorSelector.Top := 22;
    FBkColorSelector.Width := 20;
    FBkColorSelector.Height := 20;
    FBkColorSelector.Visible := true;
    btnx := btnx + 20;
  end;

  if oAlignLeft in Options then
  begin
    FAlignLeftButton.Parent := FToolBar;
    FAlignLeftButton.Left := btnx;
    FAlignLeftButton.Top := 22;
    FAlignLeftButton.Width := 20;
    FAlignLeftButton.Height := 20;
    FAlignLeftButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oAlignCenter in Options then
  begin
    FAlignCenterButton.Parent := FToolBar;
    FAlignCenterButton.Left := btnx;
    FAlignCenterButton.Top := 22;
    FAlignCenterButton.Width := 20;
    FAlignCenterButton.Height := 20;
    FAlignCenterButton.Visible := true;
    btnx := btnx + 20;
  end;

  if oAlignRight in Options then
  begin
    FAlignRightButton.Parent := FToolBar;
    FAlignRightButton.Left := btnx;
    FAlignRightButton.Top := 22;
    FAlignRightButton.Width := 20;
    FAlignRightButton.Height := 20;
    FAlignRightButton.Visible := true;
    btnx := btnx + 20;
  end;

  FFontSelector.Parent := FToolBar;
  FFontSelector.Left := 0;
  FFontSelector.Top := 0;
  FFontSelector.Width := 120;
  FFontSelector.Visible := true;

  FFontSizeSelector.Parent := FToolBar;
  FFontSizeSelector.Left := 120;
  FFontSizeSelector.Top := 0;
  FFontSizeSelector.Width := 40;
  FFontSizeSelector.Visible := true;

  TAdvToolBarEx(FToolBar).HideDragGrip := true;

  Width := Max(160, btnx);
  Height := 44;
  FToolBar.Width := Width;
end;

procedure TAdvPopupToolBarWindow.Paint;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clWhite;
  Canvas.FillRect(ClientRect);
end;

procedure TAdvPopupToolBarWindow.SetColor(AColor: TColor);
begin
  FBkColorSelector.SelectedColor := AColor;
end;

procedure TAdvPopupToolBarWindow.SetFont(AName: string);
begin
  FFontSelector.SelectedFontName := AName;
end;

procedure TAdvPopupToolBarWindow.SetFontColor(AColor: TColor);
begin
  FTxtColorSelector.SelectedColor := AColor;
end;

procedure TAdvPopupToolBarWindow.SetFontSize(ASize: integer);
begin
  FFontSizeSelector.SelectedFontSize := ASize;
end;

procedure TAdvPopupToolBarWindow.SetFontStyleState(AType: TFontStyleType;
  State: boolean);
begin
  case AType of
    fstBold: FBoldButton.Down := State;
    fstItalic: FItalicButton.Down := State;
    fstUnderline: FUnderlineButton.Down := State;
    fstStrikeThrough: FStrikethroughButton.Down := State;
    fstSubScript: FSubScriptButton.Down := State;
    fstSuperScript: FSuperScriptButton.Down := State;
  end;
end;

procedure TAdvPopupToolBarWindow.SetToolBarStyler(
  const Value: TCustomAdvToolBarStyler);
begin
  FToolBarStyler := Value;
  FToolBar.ToolBarStyler := Value;
end;

procedure TAdvPopupToolBarWindow.WMActivate(var Message: TMessage);
begin
  inherited;
  message.Result := 1;
end;

procedure TAdvPopupToolBarWindow.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

end.
