{*************************************************************************}
{ TMS AdvPreviewMenuDE component                                          }
{ for Delphi & C++Builder                                                 }
{ version 1.1                                                             }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2007                                              }
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

{$I TMSDEFS.INC}

unit AdvPreviewMenuDE;

interface

uses
  Classes, Controls, AdvPreviewMenu, AdvPreviewMenuEditor, Forms, dialogs, Menus
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;


type

  TAdvPreviewMenuEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TPreviewMenuProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;


implementation

uses
  SysUtils;

//------------------------------------------------------------------------------

{$IFNDEF DELPHI6_LVL}
procedure TAdvPreviewMenuEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean); 
{$ELSE}
procedure TAdvPreviewMenuEditor.EditProperty(const PropertyEditor:IProperty; var Continue:Boolean);
{$ENDIF}
{var
  PropName: string;}
begin
  ExecuteVerb(0);
  Continue := False;
  {PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'MENUITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;}
end;

//------------------------------------------------------------------------------

function GetParentForm(AComponent: TComponent): TComponent;
begin
  Result := AComponent.Owner;
end;

//------------------------------------------------------------------------------

procedure LoadAction(PreviewMenu: TAdvPreviewMenu; MenuEditor: TPreviewMenuEditor);
var
  f: TComponent;
  i: Integer;
begin
  if Assigned(MenuEditor) then
  begin
    f := GetParentForm(PreviewMenu);
    if (f is TForm) then
    begin
      MenuEditor.cmb_Action.Clear;
      MenuEditor.cmb_SubAction.Clear;
      for i := 0 to TForm(f).ComponentCount-1 do
      begin
        if (TForm(f).Components[i] is TBasicAction) then
        begin
          MenuEditor.cmb_Action.Items.AddObject(TForm(f).Components[i].Name, TBasicAction(TForm(f).Components[i]));
          MenuEditor.cmb_SubAction.Items.AddObject(TForm(f).Components[i].Name, TBasicAction(TForm(f).Components[i]));
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure LoadSubMenu(PreviewMenu: TAdvPreviewMenu; MenuEditor: TPreviewMenuEditor);
var
  f: TComponent;
  i: Integer;
begin
  if Assigned(MenuEditor) then
  begin
    f := GetParentForm(PreviewMenu);
    if (f is TForm) then
    begin
      MenuEditor.cmb_SubMenu.Clear;
      for i := 0 to TForm(f).ComponentCount-1 do
      begin
        if (TForm(f).Components[i] is TPopupMenu) then
        begin
          MenuEditor.cmb_SubMenu.Items.AddObject(TForm(f).Components[i].Name, TPopupMenu(TForm(f).Components[i]));
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvPreviewMenuEditor.ExecuteVerb(Index: integer);
var
  MenuEditor: TPreviewMenuEditor;
  PreviewMenu: TAdvPreviewMenu;
begin
  case Index of
    0:
    begin
      if (Component is TAdvPreviewMenu) then
      begin
        PreviewMenu := TAdvPreviewMenu(Component);
        MenuEditor := TPreviewMenuEditor.Create(Application);
        MenuEditor.PreviewMenuControl.AssignMenu(PreviewMenu);
        LoadAction(PreviewMenu, MenuEditor);
        LoadSubMenu(PreviewMenu, MenuEditor);
        try
          if MenuEditor.Showmodal = mrOK then
          begin
            PreviewMenu.MenuItems.Assign(MenuEditor.PreviewMenuControl.AdvPreviewMenu.MenuItems);
            PreviewMenu.SubMenuItems.Assign(MenuEditor.PreviewMenuControl.AdvPreviewMenu.SubMenuItems);
            Designer.Modified;
          end;
        finally
          MenuEditor.Free;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvPreviewMenuEditor.GetVerb(index: integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'Menu Editor';
  end;
end;

//------------------------------------------------------------------------------

function TAdvPreviewMenuEditor.GetVerbCount: integer;
begin
 Result := 1;
end;

//------------------------------------------------------------------------------

{ TPreviewMenuProperty }

function TPreviewMenuProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuProperty.Edit;
var
  MenuEditor: TPreviewMenuEditor;
  PreviewMenu: TAdvPreviewMenu;
begin
  if (GetComponent(0) is TAdvPreviewMenu) then
  begin
    PreviewMenu := TAdvPreviewMenu(GetComponent(0));
    MenuEditor := TPreviewMenuEditor.Create(Application);
    MenuEditor.PreviewMenuControl.AssignMenu(PreviewMenu);
    try
      if MenuEditor.Showmodal = mrOK then
      begin
        PreviewMenu.MenuItems.Assign(MenuEditor.PreviewMenuControl.AdvPreviewMenu.MenuItems);
        PreviewMenu.SubMenuItems.Assign(MenuEditor.PreviewMenuControl.AdvPreviewMenu.SubMenuItems);
        Modified;
        //SetStrValue(s);
      end;
    finally
      MenuEditor.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreviewMenuProperty.SetValue(const Value: String);
begin
end;

//------------------------------------------------------------------------------

function TPreviewMenuProperty.GetValue: String;
begin
  Result:='(PreviewMenuItems)';
end;

//------------------------------------------------------------------------------

end.