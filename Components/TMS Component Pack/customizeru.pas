{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{ version 1.4                                                             }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2006 - 2009                                       }
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

unit CustomizerU;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Types,
  Dialogs, StdCtrls, ExtCtrls, Buttons, AdvToolBar, IniFiles, Math, AdvGlowButton
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TCustomizerForm = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    GroupBox2: TGroupBox;
    EdtCaption: TEdit;
    LblCaption: TLabel;
    CmBxGlphPos: TComboBox;
    LblGlphPos: TLabel;
    ChkLargeIcon: TCheckBox;
    GroupBox1: TGroupBox;
    LblCommands: TLabel;
    LblToolbarItems: TLabel;
    TopButton: TSpeedButton;
    UpButton: TSpeedButton;
    DownButton: TSpeedButton;
    BottomButton: TSpeedButton;
    AddButton: TSpeedButton;
    SeparatorButton: TSpeedButton;
    DeleteButton: TSpeedButton;
    ClearButton: TSpeedButton;
    LstBxCommand: TListBox;
    LstBxToolBarItems: TListBox;
    CmBxIcon: TComboBox;
    BtnEdit: TSpeedButton;
    BtnReset: TButton;
    AddGlowButton: TSpeedButton;
    procedure LstBxToolBarItemsDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure TopButtonClick(Sender: TObject);
    procedure UpButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure BottomButtonClick(Sender: TObject);
    procedure SeparatorButtonClick(Sender: TObject);
    procedure CmBxIconDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure OkBitBtnClick(Sender: TObject);
    procedure LstBxToolBarItemsDblClick(Sender: TObject);
    procedure LstBxToolBarItemsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure LstBxCommandClick(Sender: TObject);
    procedure LstBxToolBarItemsClick(Sender: TObject);
    procedure CmBxGlphPosChange(Sender: TObject);
    procedure LstBxToolBarItemsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LstBxToolBarItemsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure EdtCaptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChkLargeIconClick(Sender: TObject);
    procedure AddGlowButtonClick(Sender: TObject);
  private
    { Private declarations }
    FModified: Boolean;
    FLoading: Boolean;
    FCanEditBtn: Boolean;
  protected
    procedure SetModified;
    procedure UpdateComponentPos;
    procedure SetComponents;
    procedure SaveChanges;
  public
    { Public declarations }
    FToolBarCustomizer: TAdvToolBarCustomizer;
    procedure LoadComponents;
    procedure SetCaption(Value: Boolean);
    procedure SetGlyphPosition(Value: Boolean);
    property Modified: Boolean read FModified;
  end;

  TProAdvToolBarButton = class(TAdvToolBarButton);
  TProCustomGlowButton = class(TAdvCustomGlowButton);
  
var
  CustomizerForm: TCustomizerForm;

implementation
uses
  CustomizerBtnU;
  
{$R *.dfm}

//------------------------------------------------------------------------------

{ TCustomizerForm }

procedure TCustomizerForm.LoadComponents;
var
  i: integer;
  ATBItem: TATBItemProp;
begin
  if Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) then
  begin
    //--- Load AdvToolBar Items
    if Assigned(FToolBarCustomizer.AdvToolBar.Images) then
      LstBxToolBarItems.ItemHeight := FToolBarCustomizer.AdvToolBar.Images.Height + 3;

    for i:= 0 to FToolBarCustomizer.AdvToolBar.ToolBarControlCount -1 do
    begin
      if FToolBarCustomizer.AdvToolBar.ToolBarControls[i] is TAdvToolBarButton then
      begin
        ATBItem := TATBItemProp.Create;
        ATBItem.ATBItem := TAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]);
        ATBItem.CommandID := TProAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).FCommandID;
        ATBItem.CustomizerCreated := TProAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).FCustomizerCreated;
        ATBItem.Caption := TAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Caption;
        ATBItem.Hint := TAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Hint;
        ATBItem.Visible := TAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Visible;
        ATBItem.ImageIndex := TAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).ImageIndex;
        ATBItem.ForceImageIndex := TProAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).ForceImageIndex;
        ATBItem.ShowCaption := TAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).ShowCaption;
        LstBxToolBarItems.Items.AddObject(TAdvToolBarButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Caption, ATBItem);
      end
      else if FToolBarCustomizer.AdvToolBar.ToolBarControls[i] is TAdvToolBarSeparator then
      begin
        ATBItem := TATBItemProp.Create;
        ATBItem.ATBItem := FToolBarCustomizer.AdvToolBar.ToolBarControls[i];
        LstBxToolBarItems.Items.AddObject('Separator', ATBItem);
      end
      else if (FToolBarCustomizer.AdvToolBar.ToolBarControls[i] is TAdvCustomGlowButton) then
      begin
        ATBItem := TATBItemProp.Create;
        ATBItem.ATBItem := TAdvCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]);
        ATBItem.CommandID := TProCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).FCommandID;
        ATBItem.CustomizerCreated := TProCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).FCustomizerCreated;
        ATBItem.Caption := TAdvCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Caption;
        ATBItem.Hint := TAdvCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Hint;
        ATBItem.Visible := TAdvCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Visible;
        ATBItem.ImageIndex := TAdvCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).ImageIndex;
        ATBItem.ForceImageIndex := False;
        ATBItem.ShowCaption := TAdvCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).ShowCaption;
        LstBxToolBarItems.Items.AddObject(TAdvCustomGlowButton(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Caption, ATBItem);
      end
      else if FToolBarCustomizer.AdvToolBar.ToolBarControls[i] is TControl then
      begin
        LstBxToolBarItems.Items.AddObject(TControl(FToolBarCustomizer.AdvToolBar.ToolBarControls[i]).Name, nil);
      end;
    end;

    //--- Load Commands
    for i := 0 to FToolBarCustomizer.Commands.Count -1 do
    begin
      if (FToolBarCustomizer.Commands[i].ItemIndex >= 0) then
        LstBxCommand.Items.AddObject(FToolBarCustomizer.Commands[i].Caption, Pointer(FToolBarCustomizer.Commands[i].ItemIndex+1))
      else
        LstBxCommand.Items.AddObject(FToolBarCustomizer.Commands[i].Caption, nil);
    end;

    //--- Load Icon
    if Assigned(FToolBarCustomizer.AdvToolBar.Images) then
    begin
      CmBxIcon.ItemHeight := FToolBarCustomizer.AdvToolBar.Images.Height + 3;
      for i := 0 to FToolBarCustomizer.AdvToolBar.Images.count-1 do
        CmBxIcon.Items.Add('');
    end;

    //CmBxIcon.Items.Add('None');
    CmBxIcon.Items.Add(FToolBarCustomizer.DialogSettings.GlyphEditNoneIconCaption);


    CmBxIcon.Visible := Assigned(FToolBarCustomizer.AdvToolBar.Images) and (FToolBarCustomizer.AdvToolBar.Images.Count > 0) and FToolBarCustomizer.DialogSettings.EditGlyphVisible;
    if CmBxIcon.Visible then
      LstBxCommand.Height := 193
    else
      LstBxCommand.Height := 219;

    EdtCaption.Text := FToolBarCustomizer.AdvToolBar.caption;
    CmBxGlphPos.ItemIndex := 0;
    for i:= 0 to FToolBarCustomizer.AdvToolBar.ToolBarControlCount-1 do
    begin
      if FToolBarCustomizer.AdvToolBar.ToolBarControls[i] is TAdvToolBarButton then
      begin
        CmBxGlphPos.ItemIndex := Integer((FToolBarCustomizer.AdvToolBar.ToolBarControls[i] as TAdvToolBarButton).GlyphPosition);
        break;
      end;
    end;

    ChkLargeIcon.Checked := Assigned(FToolBarCustomizer.LargeImages) and (FToolBarCustomizer.LargeImages = FToolBarCustomizer.AdvToolBar.Images);
    if LstBxCommand.Items.Count > 0 then
    begin
      LstBxCommand.ItemIndex := 0;
      LstBxCommandClick(LstBxCommand);
    end;

    if LstBxToolBarItems.Items.Count > 0 then
      LstBxToolBarItems.ItemIndex := 0;

    FCanEditBtn := FToolBarCustomizer.ButtonProperties.Caption or FToolBarCustomizer.ButtonProperties.Hint or FToolBarCustomizer.ButtonProperties.Glyph
                       or FToolBarCustomizer.ButtonProperties.ShowCaption or FToolBarCustomizer.ButtonProperties.Visible;
    UpdateComponentPos;
    SetComponents;
    FLoading := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.UpdateComponentPos;
begin
  LblCaption.Visible := EdtCaption.Visible;
  LblGlphPos.Visible := CmBxGlphPos.Visible;

  {if not CmBxGlphPos.Visible then
    if ChkLargeIcon.Visible then
      ChkLargeIcon.Left := 190;
   }
  if ChkLargeIcon.Visible then
  begin
    ChkLargeIcon.Left := 50;
    ChkLargeIcon.Top := 43;
  end;
    
  if not EdtCaption.Visible then
  begin
    if CmBxGlphPos.Visible then
    begin
      LblGlphPos.Left := 6;
      CmBxGlphPos.Left := LblGlphPos.Left + LblGlphPos.Width + 5;
      ChkLargeIcon.Left := CmBxGlphPos.Left;

      if ChkLargeIcon.Visible then
      begin
        ChkLargeIcon.Left := 200;
        ChkLargeIcon.Top := 19;
      end;
    end
    else if ChkLargeIcon.Visible then
    begin
      ChkLargeIcon.Left := 7;
      ChkLargeIcon.Top := 16;
    end;
  end
  else if not CmBxGlphPos.Visible then
  begin
    if ChkLargeIcon.Visible then
    begin
      ChkLargeIcon.Left := 200;
      ChkLargeIcon.Top := 19;
    end;
  end;

  EdtCaption.Left := LblCaption.Left + LblCaption.Width + 5;
  LblGlphPos.Left := CmBxGlphPos.Left - 5 - LblGlphPos.Width;
  GroupBox2.Visible := EdtCaption.Visible or CmBxGlphPos.Visible or ChkLargeIcon.Visible;
  if not GroupBox2.Visible then
  begin
    self.Height := Self.Height - GroupBox2.Height;
    BtnOk.Top := BtnOk.Top - GroupBox2.Height;
    BtnCancel.Top := BtnCancel.Top - GroupBox2.Height;
    BtnReset.Top := BtnOk.Top;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.SetComponents;
begin
  BtnEdit.Enabled := FCanEditBtn and (LstBxToolBarItems.ItemIndex >= 0) and (LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex] <> nil) and (LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex] is TATBItemProp)
    and ((TATBItemProp(LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex]).ATBItem is TAdvToolBarButton) or (TATBItemProp(LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex]).IsATBButton)
    or ((TATBItemProp(LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex]).ATBItem is TAdvCustomGlowButton)) or (TATBItemProp(LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex]).IsGlowButton));

  TopButton.Enabled := (LstBxToolBarItems.ItemIndex > 0) and (FToolBarCustomizer.Options.ReOrder);
  UpButton.Enabled := (LstBxToolBarItems.ItemIndex > 0) and (FToolBarCustomizer.Options.ReOrder);
  DownButton.Enabled := (LstBxToolBarItems.ItemIndex >= 0) and (LstBxToolBarItems.ItemIndex < LstBxToolBarItems.Items.Count-1) and (FToolBarCustomizer.Options.ReOrder);
  BottomButton.Enabled := (LstBxToolBarItems.ItemIndex >= 0) and (LstBxToolBarItems.ItemIndex < LstBxToolBarItems.Items.Count-1) and (FToolBarCustomizer.Options.ReOrder);

  AddButton.Enabled := LstBxCommand.ItemIndex >= 0;
  DeleteButton.Enabled := LstBxToolBarItems.ItemIndex >= 0;
  ClearButton.Enabled := LstBxToolBarItems.Items.Count > 0;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.SetCaption(Value: Boolean);
begin
  EdtCaption.Visible := Value;
  LblCaption.Visible := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.SetGlyphPosition(Value: Boolean);
begin
  CmBxGlphPos.Visible := Value;
  LblGlphPos.Visible := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.LstBxToolBarItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Offset: Integer;
  ATBItem: TATBItemProp;
begin
  Offset := 2;
 with (Control as TListBox).Canvas do
  begin
    FillRect(Rect);
    if ((Control as TListBox).Items.Objects[Index] <> nil) then
    begin
      ATBItem := TATBItemProp((Control as TListBox).Items.Objects[Index]);
      if (ATBItem.ATBItem <> nil) and (ATBItem.ATBItem is TAdvToolBarButton) then
      begin
        // Assign Image here
        if Assigned(TAdvToolBarButton(ATBItem.ATBItem).Glyph) and not TAdvToolBarButton(ATBItem.ATBItem).Glyph.Empty then
        begin
          (Control as TListBox).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, TAdvToolBarButton(ATBItem.ATBItem).Glyph);
          Offset := Offset + TAdvToolBarButton(ATBItem.ATBItem).Glyph.Width;
        end
        else if (TAdvToolBarButton(ATBItem.ATBItem).ImageIndex >= 0) and Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) then
        begin
          FToolBarCustomizer.AdvToolBar.Images.Draw((Control as TListBox).Canvas, Rect.Left + 2, Rect.Top + 2, ATBItem.ImageIndex{TAdvToolBarButton(ATBItem.ATBItem).ImageIndex});
          Offset := Offset + FToolBarCustomizer.AdvToolBar.Images.Width +8;
        end
        else if Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) then
          Offset := Offset + FToolBarCustomizer.AdvToolBar.Images.Width +8;

      end
      else if ((ATBItem.ATBItem <> nil) and (ATBItem.ATBItem is TAdvToolBarSeparator)) or ((ATBItem.ATBItem = nil) and ATBItem.CustomizerCreated and ATBItem.IsSeparator) then
      begin
        Offset := Offset + 8;
      end
      else if (ATBItem.ATBItem = nil) and ATBItem.CustomizerCreated and ATBItem.IsATBButton then
      begin
        if Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) and(ATBItem.ImageIndex >= 0) then
        begin
          FToolBarCustomizer.AdvToolBar.Images.Draw((Control as TListBox).Canvas, Rect.Left + 2, Rect.Top + 2, ATBItem.ImageIndex);
          Offset := Offset + FToolBarCustomizer.AdvToolBar.Images.Width +8;
        end;
      end
      else if (ATBItem.ATBItem <> nil) and (ATBItem.ATBItem is TAdvCustomGlowButton) then
      begin
        // AdvCustomGlowButton
        if Assigned(TAdvCustomGlowButton(ATBItem.ATBItem).Picture) and not TAdvCustomGlowButton(ATBItem.ATBItem).Picture.Empty then
        begin
          (Control as TListBox).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, TAdvCustomGlowButton(ATBItem.ATBItem).Picture);
          Offset := Offset + TAdvCustomGlowButton(ATBItem.ATBItem).Picture.Width;
        end
        else if (TAdvCustomGlowButton(ATBItem.ATBItem).ImageIndex >= 0) and Assigned(FToolBarCustomizer) and Assigned(TAdvCustomGlowButton(ATBItem.ATBItem).Images) then
        begin
          TAdvCustomGlowButton(ATBItem.ATBItem).Images.Draw((Control as TListBox).Canvas, Rect.Left + 2, Rect.Top + 2, ATBItem.ImageIndex);
          Offset := Offset + TAdvCustomGlowButton(ATBItem.ATBItem).Images.Width +8;
        end;
      end
      else
      begin
        // Customizer created AdvCustomGlowButton
        if (ATBItem.ATBItem = nil) and ATBItem.CustomizerCreated and ATBItem.IsGlowButton then
        begin
          if Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) and(ATBItem.ImageIndex >= 0) then
          begin
            FToolBarCustomizer.AdvToolBar.Images.Draw((Control as TListBox).Canvas, Rect.Left + 2, Rect.Top + 2, ATBItem.ImageIndex);
            Offset := Offset + FToolBarCustomizer.AdvToolBar.Images.Width +8;
          end;
        end;
      end;
    end;
    // Calculate Text Start Position
    Offset := Offset + 8;
    // display the text
    TextOut(Rect.Left + Offset, Rect.Top+(((Control as TListBox).ItemHeight-TextHeight('Wg')) div 2),
    (Control as TListBox).Items[Index]);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.TopButtonClick(Sender: TObject);
begin
  if LstBxToolBarItems.Items.Count = 0 then
    Exit;

  with LstBxToolBarItems do
  begin
    // Move the items
    Items.Move(ItemIndex, 0);
    ItemIndex := 0;
    SetModified;
    SetComponents;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.SetModified;
begin
  if not FLoading then
    FModified := True;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.UpButtonClick(Sender: TObject);
var
  NewPosition: Integer;
begin
  if LstBxToolBarItems.Items.Count = 0 then
    Exit;

  with LstBxToolBarItems do
  begin
    if ItemIndex < 0 then
    begin
      ItemIndex := Items.Count -1;
      SetComponents;
      Exit;
    end;
    if ItemIndex > 0 then
    begin
      NewPosition := ItemIndex-1;
      // Move the items
      Items.Exchange(NewPosition, ItemIndex);
      ItemIndex := NewPosition;
      SetModified;
      SetComponents;
    end
    else
    begin
      NewPosition := Items.Count -1;
      // Move the items
      Items.Exchange(NewPosition, ItemIndex);
      ItemIndex := NewPosition;
      SetModified;
      SetComponents;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.DownButtonClick(Sender: TObject);
var
  NewPosition: Integer;
begin
  if LstBxToolBarItems.Items.Count = 0 then
    Exit;

  with LstBxToolBarItems do
  begin
    if ItemIndex = -1 then
    begin
      ItemIndex := 0;
      SetComponents;
      Exit;
    end;

    if ItemIndex < Items.Count -1 then
    begin
      NewPosition := ItemIndex+1;
      // Move the items
      Items.Exchange(NewPosition, ItemIndex);
      ItemIndex := NewPosition;
    end
    else
    begin
      NewPosition := 0;
      // Move the items
      Items.Exchange(NewPosition, ItemIndex);
      ItemIndex := NewPosition;
    end;

    SetModified;
    SetComponents;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.BottomButtonClick(Sender: TObject);
begin
  if LstBxToolBarItems.Items.Count = 0 then
    Exit;

  with LstBxToolBarItems do
  begin
    // Move the items
    Items.Move(ItemIndex, LstBxToolBarItems.Items.Count -1);
    ItemIndex := LstBxToolBarItems.Items.Count -1;

    SetModified;
    SetComponents;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.CmBxIconDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  CmBxIcon.Canvas.FillRect(Rect);

  if Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) and (Index < FToolBarCustomizer.AdvToolBar.Images.Count) then
    FToolBarCustomizer.AdvToolBar.Images.Draw(CmBxIcon.Canvas, Rect.Left + 2, Rect.Top + 2, Index)
  else if Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) and (Index = FToolBarCustomizer.AdvToolBar.Images.Count) then
  begin
    CmBxIcon.Canvas.TextOut(Rect.Left + 4, Rect.Top+((CmBxIcon.ItemHeight- CmBxIcon.Canvas.TextHeight('Wg')) div 2), CmBxIcon.Items[Index]);
  end;

end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to LstBxToolBarItems.Items.Count-1 do
  begin
    if (LstBxToolBarItems.Items.Objects[i] <> nil) and (LstBxToolBarItems.Items.Objects[i] is TATBItemProp) then
      TATBItemProp(LstBxToolBarItems.Items.Objects[i]).Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.BtnEditClick(Sender: TObject);
var
  i: Integer;
  AItem: TATBItemProp;
  p: TPoint;
begin
  CusBtnForm := TCusBtnForm.Create(self);
  CusBtnForm.FToolBarCustomizer := self.FToolBarCustomizer;
  //CusBtnForm.Caption := FToolBarCustomizer.ButtonProperties.FormCaption;

  CusBtnForm.LblBtnCaption.Caption := FToolBarCustomizer.DialogSettings.ButtonEditCaptionLabel;
  CusBtnForm.LblGlyph.Caption := FToolBarCustomizer.DialogSettings.ButtonEditIconLabel;
  CusBtnForm.chkBtnVis.Caption := FToolBarCustomizer.DialogSettings.ButtonEditVisibleCheck;
  CusBtnForm.ChkBtnShowCap.Caption := FToolBarCustomizer.DialogSettings.ButtonEditCaptionCheck;
  CusBtnForm.lblBtnHint.Caption := FToolBarCustomizer.DialogSettings.ButtonEditHintLabel;
  CusBtnForm.Caption := FToolBarCustomizer.DialogSettings.ButtonEditCaption;

  CusBtnForm.BtnOk.Caption := FToolBarCustomizer.DialogSettings.OkButtonCaption;
  CusBtnForm.BtnOk.Hint := FToolBarCustomizer.DialogSettings.OkButtonHint;
  CusBtnForm.BtnCancel.Caption := FToolBarCustomizer.DialogSettings.CancelButtonCaption;
  CusBtnForm.BtnCancel.Hint := FToolBarCustomizer.DialogSettings.CancelButtonHint;

  //--- Load Icon
  CusBtnForm.CmBxGlyph.Visible := False;
  if FToolBarCustomizer.ButtonProperties.Glyph and Assigned(FToolBarCustomizer.AdvToolBar.Images) then
  begin
    CusBtnForm.CmBxGlyph.ItemHeight := FToolBarCustomizer.AdvToolBar.Images.Height + 3;
    for i := 0 to FToolBarCustomizer.AdvToolBar.Images.count-1 do
      CusBtnForm.CmBxGlyph.Items.Add('');

    //CusBtnForm.CmBxGlyph.Items.Add('None');
    CusBtnForm.CmBxGlyph.Items.Add(FToolBarCustomizer.DialogSettings.GlyphEditNoneIconCaption);

    CusBtnForm.CmBxGlyph.Visible := True;
  end;

  if (LstBxToolBarItems.ItemIndex >= 0) and (LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex] <> nil) and (LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex] is TATBItemProp) then
  begin
    AItem := (LstBxToolBarItems.Items.Objects[LstBxToolBarItems.ItemIndex] as TATBItemProp);
    CusBtnForm.EdtBtnCaption.Text := AItem.Caption; // LstBxToolBarItems.Items[LstBxToolBarItems.itemIndex];
    CusBtnForm.EdtBtnHint.Text := AItem.Hint;
    CusBtnForm.ChkBtnShowCap.Checked := AItem.ShowCaption;
    CusBtnForm.ChkBtnVis.Checked := AItem.Visible;
    CusBtnForm.CmBxGlyph.Enabled := True;
    if (AItem.ATBItem is TAdvCustomGlowButton) and not TProCustomGlowButton(AItem.ATBItem).FCustomizerCreated then
    begin
      CusBtnForm.CmBxGlyph.Enabled := False;
      CusBtnForm.CmBxGlyph.Text := '';
    end;
      
    if CusBtnForm.CmBxGlyph.Enabled and (AItem.ImageIndex < CusBtnForm.CmBxGlyph.Items.count) then
      CusBtnForm.CmBxGlyph.ItemIndex := AItem.ImageIndex;
  end;

  //--- Set Component Visibility
  CusBtnForm.EdtBtnCaption.Enabled := FToolBarCustomizer.ButtonProperties.Caption;
  CusBtnForm.LblBtnCaption.Visible := CusBtnForm.EdtBtnCaption.Visible;
  CusBtnForm.EdtBtnHint.Visible := FToolBarCustomizer.ButtonProperties.Hint;
  CusBtnForm.LblBtnHint.Visible := CusBtnForm.EdtBtnHint.Visible;
  CusBtnForm.CmBxGlyph.Visible := FToolBarCustomizer.ButtonProperties.Glyph;
  CusBtnForm.LblGlyph.Visible := CusBtnForm.CmBxGlyph.Visible;
  CusBtnForm.ChkBtnShowCap.Visible := FToolBarCustomizer.ButtonProperties.ShowCaption;
  CusBtnForm.ChkBtnVis.Visible := FToolBarCustomizer.ButtonProperties.Visible;

  //--- Set component Pos
  if not CusBtnForm.EdtBtnHint.Visible then
    if CusBtnForm.CmBxGlyph.Visible then
    begin
      CusBtnForm.EdtBtnHint.Left := 59;
      CusBtnForm.EdtBtnHint.Top := 32;
      CusBtnForm.LblBtnHint.Left := 35;
      CusBtnForm.LblBtnHint.Top := 36;
    end;

  if not CusBtnForm.ChkBtnShowCap.Visible then
    if CusBtnForm.ChkBtnVis.Visible then
    begin
      CusBtnForm.ChkBtnVis.Top := 11;
      CusBtnForm.ChkBtnVis.Left := 139;
    end;

  i := 0;
  if CusBtnForm.EdtBtnCaption.Visible then
    i := CusBtnForm.LblBtnCaption.Width;
  if CusBtnForm.EdtBtnHint.Visible then
    i := Max(i, CusBtnForm.LblBtnHint.Width);
  if CusBtnForm.CmBxGlyph.Visible then
    i := Max(i, CusBtnForm.LblGlyph.Width);

  if i > 0 then
  begin
    i := i + 7 + 5;
    CusBtnForm.EdtBtnCaption.Left := i;
    CusBtnForm.LblBtnCaption.Left := i - 5 - CusBtnForm.LblBtnCaption.Width;
    CusBtnForm.EdtBtnHint.Left := i;
    CusBtnForm.LblBtnHint.Left := i - 5 - CusBtnForm.LblBtnHint.Width;
    CusBtnForm.CmBxGlyph.Left := i;
    CusBtnForm.LblGlyph.Left := i - 5 - CusBtnForm.LblGlyph.Width;

    CusBtnForm.ChkBtnShowCap.Left := Max(CusBtnForm.ChkBtnShowCap.Left, i + CusBtnForm.EdtBtnCaption.Width + 4);
    CusBtnForm.ChkBtnVis.Left := CusBtnForm.ChkBtnShowCap.Left;

    i := CusBtnForm.ChkBtnShowCap.Left + CusBtnForm.EdtBtnCaption.Width;
    if ( i >= CusBtnForm.GroupBox1.Width) then
    begin
      CusBtnForm.GroupBox1.Width := CusBtnForm.GroupBox1.Width + (i - CusBtnForm.GroupBox1.Width)+ 3;
      i := CusBtnForm.GroupBox1.Left + CusBtnForm.GroupBox1.Width;
      if CusBtnForm.Width <= i then
      begin
        CusBtnForm.Width := CusBtnForm.Width + (i - CusBtnForm.Width) + 6;
      end;
    end;

    i := CusBtnForm.Width div 2;
    CusBtnForm.BtnOk.Left := i - CusBtnForm.BtnOk.Width;
    CusBtnForm.BtnCancel.Left := CusBtnForm.BtnOk.Left + CusBtnForm.BtnOk.Width;
  end;

  p := Point(75, 120);
  P := ClientToScreen(P);
  CusBtnForm.Top :=  P.Y;
  CusBtnForm.left := P.X;

  i := LstBxToolBarItems.ItemIndex;
  if (CusBtnForm.ShowModal = mrOk) and (i >= 0) then
  begin
    if (LstBxToolBarItems.Items.Objects[i] <> nil) then
    begin
      if (LstBxToolBarItems.Items.Objects[i] is TATBItemProp) then
      begin
        if (((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem <> nil) and (((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem is TAdvToolBarButton))
           or ((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem is TAdvCustomGlowButton))
           or (((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).CustomizerCreated) and (((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).IsATBButton) or ((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).IsGlowButton)) ) then
        begin
          AItem := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
          AItem.Caption := CusBtnForm.EdtBtnCaption.Text;
          AItem.Hint := CusBtnForm.EdtBtnHint.Text;
          AItem.Visible := CusBtnForm.ChkBtnVis.Checked;;
          AItem.ShowCaption := CusBtnForm.ChkBtnShowCap.Checked;
          if FToolBarCustomizer.ButtonProperties.Glyph and CusBtnForm.CmBxGlyph.Enabled then
          begin
            if (AItem.ImageIndex <> CusBtnForm.CmBxGlyph.ItemIndex) then
              AItem.ForceImageIndex := True;
            if (CusBtnForm.CmBxGlyph.ItemIndex < CusBtnForm.CmBxGlyph.Items.count - 1) then
              AItem.ImageIndex := CusBtnForm.CmBxGlyph.ItemIndex
            else
              AItem.ImageIndex := -1;
          end;

          LstBxToolBarItems.Items[i] := AItem.Caption;

          SetModified;
        end
      end;
    end
  end;

  CusBtnForm.Free;
  CusBtnForm := nil;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.SaveChanges;
var
  i: integer;
  IniFile: TIniFile;
  Item: TATBItemProp;
  SL, SL1: TStrings;
  N: string;

  function GetUniqueName(AOwner: TComponent; S: string): string;
  var
    i, j: integer;
    Found: boolean;
  begin
    Result := S;
    i := 1;
    Found := false;
    while not found do
    begin
      Result := S + inttostr(i);
      if Assigned(AOwner) then
      begin
        if AOwner.FindComponent(Result) = nil then
          Found := true;
      end;

      if Found then
      begin
        j := SL1.IndexOf(Result);
        if (j >= 0) then
          Found := False;
      end;

      if Found then
        SL1.Add(Result);
      inc(i);
    end;
  end;
  
begin
  if (FToolBarCustomizer.EnablePersistence) and (FToolBarCustomizer.FileName <>'') then
  begin
    // Populate Name List which is used to get unique Name
    SL1 := TStringList.Create;
    for I := 0 to LstBxToolBarItems.Items.Count-1 do
    begin
      if (TControl(LstBxToolBarItems.Items.Objects[i]) <> nil) then
      begin
        if (LstBxToolBarItems.Items.Objects[i] is TATBItemProp) then
        begin
          if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem <> nil then
          begin
            Item := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
            SL1.Add(TControl(Item.ATBItem).Name);
          end;
        end;
      end;
    end;
    //---

    IniFile := TIniFile.Create(FToolBarCustomizer.FileName);
    with IniFile do
    begin
      //--- Delete Old values
      SL := TStringList.Create;
      IniFile.ReadSectionValues(FToolBarCustomizer.Name, SL);
      for i:= 0 to SL.Count-1 do
        IniFile.DeleteKey(FToolBarCustomizer.Name, SL.Names[i]);
      SL.Free;

      if EdtCaption.Visible then
        WriteString(FToolBarCustomizer.Name {Section}, 'ATBCaption', EdtCaption.text);
      if CmBxGlphPos.Visible then
        WriteInteger(FToolBarCustomizer.Name {Section}, 'GlyphPosition', CmBxGlphPos.ItemIndex);
      if ChkLargeIcon.Visible then
        WriteInteger(FToolBarCustomizer.Name {Section}, 'LargeIcon', integer(ChkLargeIcon.Checked));

      WriteInteger(FToolBarCustomizer.Name {Section}, 'ToolBarItemCount', LstBxToolBarItems.Items.Count);

      for I := 0 to LstBxToolBarItems.Items.Count-1 do
      begin
        if (TControl(LstBxToolBarItems.Items.Objects[i]) <> nil) then
        begin
          if (LstBxToolBarItems.Items.Objects[i] is TATBItemProp) then
          begin
            if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem <> nil then
            begin
              if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem is TAdvToolBarButton then
              begin
                Item := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i), TControl(Item.ATBItem).Name);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ClassName', 'ADVTOOLBARBUTTON');
                if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).CustomizerCreated then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.CommandID', Item.CommandID);
                if FToolBarCustomizer.ButtonProperties.Glyph then
                begin
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ImageIndex', Item.ImageIndex);
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ForceImageIndex', Integer(Item.ForceImageIndex));
                end;
                if FToolBarCustomizer.ButtonProperties.Visible then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Visible', Integer(Item.Visible));
                if FToolBarCustomizer.ButtonProperties.ShowCaption then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ShowCaption', Integer(Item.ShowCaption));
                if FToolBarCustomizer.ButtonProperties.Caption then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Caption', Item.Caption);
                if FToolBarCustomizer.ButtonProperties.Hint then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Hint', Item.Hint);
              end
              else if ((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem is TAdvToolBarSeparator) then
              begin
                Item := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i), TControl(Item.ATBItem).Name);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ClassName', 'ADVTOOLBARSEPARATOR');
              end
              else if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem is TAdvCustomGlowButton then
              begin
                Item := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i), TControl(Item.ATBItem).Name);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ClassName', 'ADVCUSTOMGLOWBUTTON');
                if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).CustomizerCreated then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.CommandID', Item.CommandID);
                if FToolBarCustomizer.ButtonProperties.Glyph then
                begin
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ImageIndex', Item.ImageIndex);
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ForceImageIndex', Integer(Item.ForceImageIndex));
                end;
                if FToolBarCustomizer.ButtonProperties.Visible then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Visible', Integer(Item.Visible));
                if FToolBarCustomizer.ButtonProperties.ShowCaption then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ShowCaption', Integer(Item.ShowCaption));
                if FToolBarCustomizer.ButtonProperties.Caption then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Caption', Item.Caption);
                if FToolBarCustomizer.ButtonProperties.Hint then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Hint', Item.Hint);
              end;
            end
            else if ((LstBxToolBarItems.Items.Objects[i] as TATBItemProp).ATBItem = nil) and (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).CustomizerCreated then
            begin
              if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).IsATBButton then
              begin
                Item := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
                N := 'ADVTOOLBARBUTTON';
                N := GetUniqueName(FToolBarCustomizer.AdvToolBar, N);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i), N{LstBxToolBarItems.Items[i]});
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ClassName', 'ADVTOOLBARBUTTON');
                WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.CommandID', Item.CommandID);
                if FToolBarCustomizer.ButtonProperties.Glyph then
                begin
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ImageIndex', Item.ImageIndex);
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ForceImageIndex', Integer(Item.ForceImageIndex));
                end;
                if FToolBarCustomizer.ButtonProperties.Visible then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Visible', Integer(Item.Visible));
                if FToolBarCustomizer.ButtonProperties.ShowCaption then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ShowCaption', Integer(Item.ShowCaption));
                if FToolBarCustomizer.ButtonProperties.Caption then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Caption', Item.Caption);
                if FToolBarCustomizer.ButtonProperties.Hint then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Hint', Item.Hint);
              end
              else if (LstBxToolBarItems.Items.Objects[i] as TATBItemProp).IsGlowButton then
              begin
                Item := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i), LstBxToolBarItems.Items[i]);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ClassName', 'ADVCUSTOMGLOWBUTTON');
                WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.CommandID', Item.CommandID);
                if FToolBarCustomizer.ButtonProperties.Glyph then
                begin
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ImageIndex', Item.ImageIndex);
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ForceImageIndex', Integer(Item.ForceImageIndex));
                end;
                if FToolBarCustomizer.ButtonProperties.Visible then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Visible', Integer(Item.Visible));
                if FToolBarCustomizer.ButtonProperties.ShowCaption then
                  WriteInteger(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ShowCaption', Integer(Item.ShowCaption));
                if FToolBarCustomizer.ButtonProperties.Caption then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Caption', Item.Caption);
                if FToolBarCustomizer.ButtonProperties.Hint then
                  WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.Hint', Item.Hint);
              end
              else
              begin
                Item := (LstBxToolBarItems.Items.Objects[i] as TATBItemProp);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i), Item.Caption);
                WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ClassName', 'ADVTOOLBARSEPARATOR');
              end;
            end;
          end;
        end
        else
        begin
          WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i), LstBxToolBarItems.Items[i]);
          WriteString(FToolBarCustomizer.Name {Section}, 'Name'+ inttostr(i)+'.ClassName', '-1');
        end;
      end;
    end;
    IniFile.Free;
    SL1.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.OkBitBtnClick(Sender: TObject);
begin
  SaveChanges;
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.LstBxToolBarItemsDblClick(Sender: TObject);
begin
  if BtnEdit.Enabled and BtnEdit.Visible then
    BtnEditClick(BtnEdit);
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.LstBxToolBarItemsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetComponents;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.AddButtonClick(Sender: TObject);
var
  ATBItem: TATBItemProp;
  i: Integer;
begin
  //--- Add Item
  if (LstBxCommand.ItemIndex < 0) then
    Exit;

  ATBItem := TATBItemProp.Create;
  ATBItem.IsATBButton := True;
  ATBItem.CustomizerCreated := True;
  ATBItem.ImageIndex := CmBxIcon.ItemIndex;
  ATBItem.ForceImageIndex := True;
  ATBItem.Caption := LstBxCommand.Items[LstBxCommand.ItemIndex];
  ATBItem.Hint := ATBItem.Caption;
  ATBItem.Visible := True;
  ATBItem.CommandID := LstBxCommand.ItemIndex;
  if (LstBxToolBarItems.ItemIndex >= 0) then
  begin
    if (LstBxToolBarItems.ItemIndex < LstBxToolBarItems.Items.Count -1) then
    begin
      i := LstBxToolBarItems.ItemIndex+1;
      LstBxToolBarItems.Items.InsertObject(i, LstBxCommand.Items[LstBxCommand.ItemIndex], ATBItem);
    end
    else
    begin
      LstBxToolBarItems.Items.AddObject(LstBxCommand.Items[LstBxCommand.ItemIndex], ATBItem);
      i := LstBxToolBarItems.Items.Count-1;
    end;
  end
  else
  begin
    LstBxToolBarItems.Items.AddObject(LstBxCommand.Items[LstBxCommand.ItemIndex], ATBItem);
    i := LstBxToolBarItems.Items.Count-1;
  end;
  //i := LstBxToolBarItems.Items.IndexOf(LstBxCommand.Items[LstBxCommand.ItemIndex]);
  if (i >= 0) then
    LstBxToolBarItems.ItemIndex := i;

  SetModified;
  SetComponents;
end;

//------------------------------------------------------------------------------

function GetUniqueControlName(AControl: TWinControl; Start: String): String;
var
  i, j: Integer;
  NotFound: Boolean;
begin
  Result := Start;
  j := 1;
  NotFound := True;
  while NotFound do
  begin
    NotFound := False;
    Result := Start + InttoStr(j);
    for i:= 0 to AControl.ControlCount-1 do
    begin
      if UpperCase(AControl.Controls[i].Name) = UpperCase(Result) then
      begin
        NotFound := True;
        Break;
      end;
    end;
    Inc(j);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.SeparatorButtonClick(Sender: TObject);
var
  ATBItem: TATBItemProp;
  i: Integer;
begin
  //--- Add Separator
  ATBItem := TATBItemProp.Create;
  ATBItem.IsSeparator := True;
  ATBItem.CustomizerCreated := True;
  ATBItem.Caption := GetUniqueControlName(FToolBarCustomizer.AdvToolBar, 'AdvToolBarSeparator'); //+ InttoStr(LstBxToolBarItems.count);
  if (LstBxToolBarItems.ItemIndex >= 0) then
  begin
    if (LstBxToolBarItems.ItemIndex < LstBxToolBarItems.Items.Count-1) then
    begin
      i := LstBxToolBarItems.ItemIndex+1;
      LstBxToolBarItems.Items.InsertObject(i, 'Separator', ATBItem);
    end
    else
    begin
      LstBxToolBarItems.Items.AddObject('Separator', ATBItem);
      i := LstBxToolBarItems.Items.Count-1;
    end;
  end
  else
  begin
    LstBxToolBarItems.Items.AddObject('Separator', ATBItem);
    i := LstBxToolBarItems.Items.Count-1;
  end;

  if (i >= 0) and (i < LstBxToolBarItems.Items.Count) then
    LstBxToolBarItems.ItemIndex := i;

  SetModified;
  SetComponents;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.DeleteButtonClick(Sender: TObject);
var
  i: Integer;
begin
  i := LstBxToolBarItems.ItemIndex;

  if LstBxToolBarItems.ItemIndex >= 0 then
    LstBxToolBarItems.Items.Delete(LstBxToolBarItems.ItemIndex);

  if (i >= 0) and (i < LstBxToolBarItems.Items.Count) then
    LstBxToolBarItems.ItemIndex := i
  else if (LstBxToolBarItems.Items.Count > 0) then
    LstBxToolBarItems.ItemIndex := LstBxToolBarItems.Items.Count - 1;

  SetModified;
  SetComponents;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.ClearButtonClick(Sender: TObject);
begin
  if MessageDlg('Are you sure that you want to clear ToolBar?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  LstBxToolBarItems.Clear;

  SetModified;
  SetComponents;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.LstBxCommandClick(Sender: TObject);
var
  i: Integer;
begin
  if LstBxCommand.ItemIndex >=0 then
  begin
    if LstBxCommand.Items.Objects[LstBxCommand.ItemIndex] = nil then
      i := CmBxIcon.Items.Count -1
    else
      i := Integer(LstBxCommand.Items.Objects[LstBxCommand.ItemIndex])-1;
    if (i >= 0) and (i < CmBxIcon.Items.Count) then
      CmBxIcon.ItemIndex := i;
  end;
  SetComponents;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.LstBxToolBarItemsClick(Sender: TObject);
begin
  SetComponents;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.CmBxGlphPosChange(Sender: TObject);
begin
  SetModified;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.LstBxToolBarItemsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = LstBxToolBarItems) or (Source = LstBxCommand);
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.LstBxToolBarItemsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  i, d: Integer;
begin
  if (Source = LstBxToolBarItems) then
  begin
    i := LstBxToolBarItems.ItemIndex;
    d := LstBxToolBarItems.ItemAtPos(Point(X, Y), True);
    if (i >= 0) and (d >= 0) then
    begin
      LstBxToolBarItems.Items.Move(i, d);
      SetModified;
    end;
  end
  else if (Source = LstBxCommand) and (LstBxCommand.ItemIndex >= 0) then
  begin
    AddButtonClick(AddButton);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomizerForm.EdtCaptionChange(Sender: TObject);
begin
  SetModified;
end;

procedure TCustomizerForm.FormCreate(Sender: TObject);
begin
  FLoading := True;
end;

procedure TCustomizerForm.ChkLargeIconClick(Sender: TObject);
begin
  SetModified;
end;


procedure TCustomizerForm.AddGlowButtonClick(Sender: TObject);
var
  ATBItem: TATBItemProp;
  i: Integer;
begin
  //--- Add Item
  if (LstBxCommand.ItemIndex < 0) then
    Exit;

  ATBItem := TATBItemProp.Create;
  ATBItem.IsGlowButton := True;
  ATBItem.CustomizerCreated := True;
  ATBItem.ImageIndex := CmBxIcon.ItemIndex;
  ATBItem.ForceImageIndex := True;
  ATBItem.Caption := LstBxCommand.Items[LstBxCommand.ItemIndex];
  ATBItem.Hint := ATBItem.Caption;
  ATBItem.Visible := True;
  ATBItem.CommandID := LstBxCommand.ItemIndex;
  if (LstBxToolBarItems.ItemIndex >= 0) then
  begin
    if (LstBxToolBarItems.ItemIndex < LstBxToolBarItems.Items.Count -1) then
    begin
      i := LstBxToolBarItems.ItemIndex+1;
      LstBxToolBarItems.Items.InsertObject(i, LstBxCommand.Items[LstBxCommand.ItemIndex], ATBItem);
    end
    else
    begin
      LstBxToolBarItems.Items.AddObject(LstBxCommand.Items[LstBxCommand.ItemIndex], ATBItem);
      i := LstBxToolBarItems.Items.Count-1;
    end;
  end
  else
  begin
    LstBxToolBarItems.Items.AddObject(LstBxCommand.Items[LstBxCommand.ItemIndex], ATBItem);
    i := LstBxToolBarItems.Items.Count-1;
  end;
  if (i >= 0) then
    LstBxToolBarItems.ItemIndex := i;

  SetModified;
  SetComponents;
end;

end.
