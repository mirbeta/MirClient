#include "cxCustomData.hpp"
#include "cxGridTableView.hpp"

#ifndef ColumnsMultiEditorsDemoH
#define ColumnsMultiEditorsDemoH

class TSkillDataSource: public TcxCustomDataSource
{
  private:
    TcxGridTableView* FTableView;
    int FCommunicationLevelCount;
    int FLanguagesCount;
    Variant Grades;
  protected:
	  int __fastcall GetRecordCount(void);
  	void __fastcall SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue);
	  Variant __fastcall GetValue(void * ARecordHandle, void * AItemHandle);
  public:
    __fastcall TSkillDataSource(TcxGridTableView* ATableView, int ACommunicationLevelCount, int ALanguagesCount);
};

const
  int SkillCount = 6;

#endif
