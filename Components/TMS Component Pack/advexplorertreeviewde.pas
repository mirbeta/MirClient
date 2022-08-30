{*************************************************************************}
{ TMS AdvExplorerTreeview component                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2008 - 2010                                       }
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

unit AdvExplorerTreeviewDE;

interface

uses
  Classes, Controls, AdvExplorerTreeview, AdvExplorerTreeviewEditor, Forms, dialogs
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;

type

  TAdvExplorerTreeviewEditor = class(TDefaultEditor)
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

  TTreeviewPropEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;


implementation

uses
  SysUtils, TypInfo, AdvExplorerTreeviewStyles, AdvStyleIF;

//------------------------------------------------------------------------------
{ TAdvExplorerTreeviewEditor }

{$IFNDEF DELPHI6_LVL}
procedure TAdvExplorerTreeviewEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean); 
{$ELSE}
procedure TAdvExplorerTreeviewEditor.EditProperty(const PropertyEditor:IProperty; var Continue:Boolean);
{$ENDIF}
var
  PropName: string;
begin
  //ExecuteVerb(0);
  //Continue := False;
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

//------------------------------------------------------------------------------

function GetParentForm(AComponent: TComponent): TComponent;
begin
  Result := AComponent.Owner;
end;

//------------------------------------------------------------------------------

procedure TAdvExplorerTreeviewEditor.ExecuteVerb(Index: integer);
var
  TreeViewEditor: TExpTreeviewEditor;
  ExplorerTreeview: TAdvExplorerTreeview;
  psf: TAdvExplorerTreeViewStyleForm;
  style: TTMSStyle;
begin
  if (Index = 0) then
  begin
    if (Component is TAdvExplorerTreeview) then
    begin
      ExplorerTreeview := TAdvExplorerTreeview(Component);
      TreeviewEditor := TExpTreeviewEditor.Create(Application);
      TreeviewEditor.ExplorerTreeview := ExplorerTreeview;
      try
        if TreeviewEditor.Showmodal = mrOK then
          Designer.Modified;
      finally
        TreeviewEditor.Free;
      end;
    end;
  end
  else if (Index = 1) then
  begin
    psf := TAdvExplorerTreeViewStyleForm.Create(Application);
    style := (Component as TCustomExplorerTreeview).GetComponentStyle;

    case style of
      tsOffice2003Blue: psf.RadioGroup1.ItemIndex := 0;
      tsOffice2003Olive: psf.RadioGroup1.ItemIndex := 1;
      tsOffice2003Silver: psf.RadioGroup1.ItemIndex := 2;
      tsOffice2003Classic: psf.RadioGroup1.ItemIndex := 3;
      tsOffice2007Luna: psf.RadioGroup1.ItemIndex := 4;
      tsOffice2007Obsidian: psf.RadioGroup1.ItemIndex := 5;
      tsOffice2007Silver: psf.RadioGroup1.ItemIndex := 6;
      tsOffice2010Blue: psf.RadioGroup1.ItemIndex := 7;
      tsOffice2010Silver: psf.RadioGroup1.ItemIndex := 8;
      tsOffice2010Black: psf.RadioGroup1.ItemIndex := 9;
      tsWindowsXP: psf.RadioGroup1.ItemIndex := 10;
      tsWindowsVista: psf.RadioGroup1.ItemIndex := 11;
      tsWindows7: psf.RadioGroup1.ItemIndex := 12;
      tsTerminal: psf.RadioGroup1.ItemIndex := 13;
      tsWindows8: psf.RadioGroup1.ItemIndex := 14;
      tsOffice2013White: psf.RadioGroup1.ItemIndex := 15;
      tsOffice2013LightGray: psf.RadioGroup1.ItemIndex := 16;
      tsOffice2013Gray: psf.RadioGroup1.ItemIndex := 17;
      tsWindows10: psf.RadioGroup1.ItemIndex := 18;
      tsOffice2016White: psf.RadioGroup1.ItemIndex := 19;
      tsOffice2016Gray: psf.RadioGroup1.ItemIndex := 20;
      tsOffice2016Black: psf.RadioGroup1.ItemIndex := 21;
    end;

    if psf.ShowModal = mrOK then
    begin
      case psf.RadioGroup1.ItemIndex of
        0: style := tsOffice2003Blue;
        1: style := tsOffice2003Olive;
        2: style := tsOffice2003Silver;
        3: style := tsOffice2003Classic;
        4: style := tsOffice2007Luna;
        5: style := tsOffice2007Obsidian;
        6: style := tsOffice2007Silver;
        7: style := tsOffice2010Blue;
        8: style := tsOffice2010Silver;
        9: style := tsOffice2010Black;
        10: style := tsWindowsXP;
        11: style := tsWindowsVista;
        12: style := tsWindows7;
        13: style := tsTerminal;
        14: style := tsWindows8;
        15: style := tsOffice2013White;
        16: style := tsOffice2013LightGray;
        17: style := tsOffice2013Gray;
        18: style := tsWindows10;
        19: style := tsOffice2016White;
        20: style := tsOffice2016Gray;
        21: style := tsOffice2016Black;
      end;
      if (Component is TAdvExplorerTreeview) then
         (Component as TAdvExplorerTreeview).SetComponentStyle(style);
         Designer.Modified;
    end;
    psf.Free;
  end;
end;

//------------------------------------------------------------------------------

function TAdvExplorerTreeviewEditor.GetVerb(index: integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'AdvExplorerTreeview Editor';
    1: Result := 'Styles';
  end;
end;

//------------------------------------------------------------------------------

function TAdvExplorerTreeviewEditor.GetVerbCount: integer;
begin
 Result := 2;
end;

//------------------------------------------------------------------------------

{ TTreeviewPropEditor }

function TTreeviewPropEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//------------------------------------------------------------------------------

procedure TTreeviewPropEditor.Edit;
var
  TreeViewEditor: TExpTreeviewEditor;
  ExplorerTreeview: TAdvExplorerTreeview;
begin
  if (GetComponent(0) is TAdvExplorerTreeview) then
  begin
    ExplorerTreeview := TAdvExplorerTreeview(GetComponent(0));
    TreeviewEditor := TExpTreeviewEditor.Create(Application);
    TreeviewEditor.ExplorerTreeview := ExplorerTreeview;
    try
      if TreeviewEditor.Showmodal = mrOK then
      begin
        //SetObjectProp(ExplorerTreeview, 'Items', ExplorerTreeview.Items);
        Modified;
        //SetStrValue(s);
      end;
    finally
      TreeviewEditor.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeviewPropEditor.SetValue(const Value: String);
begin
end;

//------------------------------------------------------------------------------

function TTreeviewPropEditor.GetValue: String;
begin
  Result:='(ExplorerTreeviewItems)';
end;

//------------------------------------------------------------------------------

end.