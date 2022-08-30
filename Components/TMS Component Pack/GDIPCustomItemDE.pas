{*************************************************************************}
{ TCustomItem Base Class Design time                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
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

unit GDIPCustomItemDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, GDIPCustomItem, Windows, Forms, Controls,
  GDIPGraphicItem, GDIPButtonBarItem, StdCtrls, ExtCtrls,
  AdvGDIP,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type

  {$IFDEF DELPHI2006_LVL}
  TPictureContainerTextProperty = class(TStringProperty, IProperty80)
  private
    maxheight, maxwidth: integer;
  protected
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit(const Host: IPropertyHost; DblClick: Boolean); reintroduce; overload;
  end;
  {$ELSE}
  TPictureContainerTextProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;
  {$ENDIF}

implementation

{ TPictureContainerTextProperty }

{$IFDEF DELPHI2006_LVL}
var
  str: TPictureContainerList;

procedure TPictureContainerTextProperty.Edit(const Host: IPropertyHost;
  DblClick: Boolean);
var
  l: TListBox;
  I: Integer;
var
  comp: TPersistent;
  own: TCustomItem;
  frm: TForm;
  pic: TAdvGDIPPicture;
begin
  comp := GetComponent(0);

  str := nil;
  if Assigned(comp) then
  begin
    if (comp is TCustomItem) then
      str := (comp as TCustomItem).GetPictures
    else if comp is TGraphicGlyphs then
    begin
      own := (comp as TGraphicGlyphs).GetGraphicOwner;
      if Assigned(own) then
      begin
        str := own.GetPictures;
      end;
    end
    else if comp is TButtonBarElement then
    begin
      own := (comp as TButtonBarElement).GetButtonBarOwner;
      if Assigned(own) then
      begin
        str := own.GetPictures;
      end;
    end;
  end;

  if Assigned(str) then
  begin
    frm := TForm.Create(nil);
    frm.BorderStyle := bsNone;
    frm.Width := 0;
    frm.Height := 0;
    frm.Visible := false;
    l := TListBox.Create(Application);
    l.Width := 0;
    l.Height := 0;
    l.Parent := frm;
    frm.Show;
    l.Height := 300;
    l.OnClick := ListBoxClick;
    l.OnDrawItem := ListBoxDrawItem;
    l.OnMeasureItem := ListBoxMeasureItem;
    l.Style := lbOwnerDrawVariable;

    maxheight := 0;
    for I := 0 to str.Count - 1 do
    begin
      pic := (str.Items[i] as TPictureContainerListItem).Picture;
      if Assigned(pic) then
      begin
        if pic.Height + 6 > maxheight then
          maxheight := pic.Height + 6;

        if pic.Width > maxwidth then
          maxwidth := pic.Width;
      end;
      l.Items.AddObject((str.Items[i] as TPictureContainerListItem).Name, pic);

    end;

    l.ItemIndex := l.Items.IndexOf(Value);
    l.Parent := nil;
    frm.Free;


    Host.DropDownControl(l);
  end;
end;
{$ENDIF}

function TPictureContainerTextProperty.GetAttributes: TPropertyAttributes;
begin
  {$IFDEF DELPHI2006_LVL}
  Result := [paValueList, paSortList, paValueEditable, paMultiSelect, paCustomDropDown];
  {$ELSE}
  Result := [paValueList, paSortList, paMultiSelect];
  {$ENDIF}
end;

{$IFDEF DELPHI2006_LVL}
procedure TPictureContainerTextProperty.ListBoxClick(Sender: TObject);
begin
  if (Sender is TListBox) then
  begin
    Value := (Sender as TListBox).Items[(Sender as TListBox).ItemIndex];
    if Assigned(str) then
      str.Free;
  end;
end;

procedure TPictureContainerTextProperty.ListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  l: TListBox;
  pic: TAdvGDIPPicture;
  th: integer;
begin
  if Control is TListBox then
  begin
    l := Control as TListBox;
    pic := (l.Items.Objects[Index] as TAdvGDIPPicture);
    if Assigned(pic) then
    begin
      pic.Draw(l.Canvas, Bounds(Rect.Left + 3 + (maxwidth - pic.Width) div 2, Rect.Top + 3 + (maxheight - pic.Height) div 2, pic.Width, pic.Height));
      th := l.Canvas.TextHeight(l.Items[Index]);
      l.Canvas.TextOut(Rect.Left + 3 + maxwidth + 3, Rect.Top + 3 + (maxheight - th) div 2, l.Items[Index]);
    end
    else
      l.Canvas.TextOut(Rect.Left, Rect.Top, l.Items[Index])
  end;
end;

procedure TPictureContainerTextProperty.ListBoxMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := maxheight;
end;

{$ELSE}

procedure TPictureContainerTextProperty.GetValues(Proc: TGetStrProc);
var
  comp: TPersistent;
  own: TCustomItem;
begin
  comp := GetComponent(0);

  if Assigned(comp) then
  begin
    if (comp is TCustomItem) then
      (comp as TCustomItem).FillPictureNames(Proc)
    else if comp is TGraphicGlyphs then
    begin
      own := (comp as TGraphicGlyphs).GetGraphicOwner;
      if Assigned(own) then
      begin
        own.FillPictureNames(Proc);
      end;
    end
    else if comp is TButtonBarElement then
    begin
      own := (comp as TButtonBarElement).GetButtonBarOwner;
      if Assigned(own) then
      begin
        own.FillPictureNames(Proc);
      end;
    end;
  end;
end;

{$ENDIF}
end.
