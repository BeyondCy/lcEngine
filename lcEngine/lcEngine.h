// ���� ifdef ���Ǵ���ʹ�� DLL �������򵥵�
// ��ı�׼�������� DLL �е������ļ��������������϶���� LCENGINE_EXPORTS
// ���ű���ġ���ʹ�ô� DLL ��
// �κ�������Ŀ�ϲ�Ӧ����˷��š�������Դ�ļ��а������ļ����κ�������Ŀ���Ὣ
// LCENGINE_API ������Ϊ�Ǵ� DLL ����ģ����� DLL ���ô˺궨���
// ������Ϊ�Ǳ������ġ�
#ifdef LCENGINE_EXPORTS
#define LCENGINE_API __declspec(dllexport)
#else
#define LCENGINE_API __declspec(dllimport)
#endif

//// �����Ǵ� lcEngine.dll ������
//class LCENGINE_API ClcEngine {
//public:
//	ClcEngine(void);
//	// TODO: �ڴ�������ķ�����
//};
//
//extern LCENGINE_API int nlcEngine;
//
//LCENGINE_API int fnlcEngine(void);
LCENGINE_API void stub(void){}