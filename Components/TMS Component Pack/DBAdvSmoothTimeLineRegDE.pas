{********************************************************************}
{ TDBAdvSmoothTimeLine component                                      }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2010                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit DBAdvSmoothTimeLineRegDE;

interface
{$I TMSDEFS.INC}
uses
  DBAdvSmoothTimeLine, Classes, DBAdvSmoothTimeLineDE, StdCtrls,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'Key',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorTime',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorColor',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorColorTo',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorAnnotation',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorFixed',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorShape',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorPicture',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionCaption',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionStartTime',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionEndTime',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionFixedPosition',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionFixedSize',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionHint',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionColor',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionColorTo',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionColorMirror',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'SectionColorMirrorTo',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorAnnotationColor',TDBAdvSmoothTimeLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBAdvSmoothTimeLineDataBinding,'IndicatorAnnotationPosition',TDBAdvSmoothTimeLineFieldNameProperty);
end;

end.

