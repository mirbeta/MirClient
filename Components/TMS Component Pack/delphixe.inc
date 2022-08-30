
{$IFDEF DELPHIXE_LVL}

function TimeSeparator: char;
begin
  Result := FormatSettings.TimeSeparator;
end;

function DateSeparator: char;
begin
  Result := FormatSettings.DateSeparator;
end;

function ThousandSeparator: char;
begin
  Result := FormatSettings.ThousandSeparator;
end;

function DecimalSeparator: char;
begin
  Result := FormatSettings.DecimalSeparator;
end;

function ShortDateFormat: string;
begin
  Result := FormatSettings.ShortDateFormat;
end;

function ShortTimeFormat: string;
begin
  Result := FormatSettings.ShortTimeFormat;
end;

function LongTimeFormat: string;
begin
  Result := FormatSettings.LongTimeFormat;
end;

function TimeAMString: string;
begin
  Result := FormatSettings.TimeAMString;
end;

function TimePMString: string;
begin
  Result := FormatSettings.TimePMString;
end;

function ListSeparator: char;
begin
  Result := FormatSettings.ListSeparator;
end;

function CurrencyString: string;
begin
  Result := FormatSettings.CurrencyString;
end;

{$ENDIF}