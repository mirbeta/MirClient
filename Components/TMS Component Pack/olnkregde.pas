{************************************************************************}
{ TODBCLINK component                                                    }
{ for Delphi & C++Builder                                                }
{ version 0.4b                                                           }
{                                                                        }
{ written by                                                             }
{   TMS Software                                                         }
{   copyright © 1996-2004                                                }
{   Email : info@tmssoftware.com                                         }
{   Web : http://www.tmssoftware.com                                     }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit olnkregde;

interface
{$I TMSDEFS.INC}
uses
  Classes,odbclink,odbcde,
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
  RegisterPropertyEditor(TypeInfo(string),TODBCLink,'DataSource',TODBCDataSourceProperty);
  RegisterPropertyEditor(TypeInfo(string),TODBCLink,'Driver',TODBCDriverProperty);
  RegisterPropertyEditor(TypeInfo(string),TODBCLink,'Table',TODBCTableProperty);
  RegisterPropertyEditor(TypeInfo(integer),TODBCField,'Datatype',TODBCFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TODBCField,'FieldName',TODBCFieldNameProperty);
end;


end.
