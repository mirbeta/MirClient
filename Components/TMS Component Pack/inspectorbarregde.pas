{***************************************************************************}
{ TInspectorBar component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}
unit InspectorBarRegDE;

interface
{$I TMSDEFS.INC}
uses
  Classes, InspectorBar, InspDE, Controls, DesignIntf, DesignEditors,
  VCLEditors, InspImg;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TInspectorBar,TInspectorBarEditor);
  RegisterPropertyEditor(TypeInfo(TInspImage), TInspectorPanel, 'Splitter', TInspImageProperty);
  RegisterPropertyEditor(TypeInfo(TInspImage), TInspectorItem, 'Background', TInspImageProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TInspectorItem, 'Shortcut', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TInspectorPanel, 'Control', TInspControlProperty);


end;

end.
