
/* from http://openglbook.com/chapter-1-getting-started.html */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <GL/glew.h>
#include <GL/freeglut.h>
#define WINDOW_TITLE_PREFIX "Chapter 2"

int CurrentWidth = 800,
   CurrentHeight = 600,
   WindowHandle = 0;
GLuint
VertexShaderId,
   FragmentShaderId,
   ProgramId,
   VaoId,
   VboId;

const GLchar* VertexShader =
{
   "#version 330\n"				\

   "layout(location=0) in vec4 in_Position;\n"	\

   "void main(void)\n"				\
   "{\n"					\
   "  gl_Position = in_Position;\n"		\
   "}\n"
};
const GLchar* FragmentShader =
{
   "#version 330\n"\

   "uniform vec4 color = vec4(1.0,0.0,0.0,1.0);\n"			\

   "out vec4 out_Color;\n"						\

   "void main(void)\n"							\
   "{\n"								\
   "  out_Color = color;\n"						\
   "}\n"
};

unsigned FrameCount = 0;

void Initialize(int, char*[]);
void InitWindow(int, char*[]);
void ResizeFunction(int, int);
void RenderFunction(void);
void TimerFunction(int);
void IdleFunction(void);
void Cleanup(void);
void CreateVBO(void);
void DestroyVBO(void);
void CreateShaders(void);
void DestroyShaders(void);

int main(int argc, char* argv[])
{
   Initialize(argc, argv);

   glutMainLoop();

   exit(EXIT_SUCCESS);
}

void Initialize(int argc, char* argv[])
{
   GLenum GlewInitResult;

   InitWindow(argc, argv);

   glewExperimental = GL_TRUE;
   GlewInitResult = glewInit();

   if (GLEW_OK != GlewInitResult) {
      fprintf(
	  stderr,
	  "ERROR: %s\n",
	  glewGetErrorString(GlewInitResult)
	      );
      exit(EXIT_FAILURE);
   }

   fprintf(
       stdout,
       "INFO: OpenGL Version: %s\n",
       glGetString(GL_VERSION)
	   );

   CreateShaders();
   CreateVBO();
   glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
}

void InitWindow(int argc, char* argv[])
{
   glutInit(&argc, argv);

   /* http://stackoverflow.com/questions/23844813/opengl-program-compiles-but-gives-error-when-run-ubuntu-14-04 */
   /*    use 3.3 context as my video card supports only that as reported by glxinfo */
   glutInitContextVersion(3, 3);
   glutInitContextFlags(GLUT_FORWARD_COMPATIBLE);
   glutInitContextProfile(GLUT_CORE_PROFILE);

   glutSetOption(
       GLUT_ACTION_ON_WINDOW_CLOSE,
       GLUT_ACTION_GLUTMAINLOOP_RETURNS
		 );

   glutInitWindowSize(CurrentWidth, CurrentHeight);

   glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);

   WindowHandle = glutCreateWindow(WINDOW_TITLE_PREFIX);

   if(WindowHandle < 1) {
      fprintf(
	  stderr,
	  "ERROR: Could not create a new rendering window.\n"
	      );
      exit(EXIT_FAILURE);
   }

   glutReshapeFunc(ResizeFunction);
   glutDisplayFunc(RenderFunction);
   glutIdleFunc(IdleFunction);
   glutTimerFunc(0, TimerFunction, 0);
   glutCloseFunc(Cleanup);
}

void ResizeFunction(int Width, int Height)
{
   CurrentWidth = Width;
   CurrentHeight = Height;
   glViewport(0, 0, CurrentWidth, CurrentHeight);
}

void RenderFunction(void)
{
   ++FrameCount;
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

   glutSwapBuffers();
   glutPostRedisplay();
}
void IdleFunction(void)
{
   glutPostRedisplay();
}

void TimerFunction(int Value)
{
   if (0 != Value) {
      char* TempString = (char*)
	    malloc(512 + strlen(WINDOW_TITLE_PREFIX));

      sprintf(
	  TempString,
	  "%s: %d Frames Per Second @ %d x %d",
	  WINDOW_TITLE_PREFIX,
	  FrameCount * 4,
	  CurrentWidth,
	  CurrentHeight
	      );

      fprintf(
	  stdout,
	  "%s: %d Frames Per Second @ %d x %d\n",
	  WINDOW_TITLE_PREFIX,
	  FrameCount * 4,
	  CurrentWidth,
	  CurrentHeight
	      );

      glutSetWindowTitle(TempString);
      free(TempString);
   }

   FrameCount = 0;
   glutTimerFunc(250, TimerFunction, 1);
}
void Cleanup(void)
{
   DestroyShaders();
   DestroyVBO();
}
void CreateVBO(void)
{
   GLfloat Vertices[] = {
      -0.8f,  0.8f, 0.0f, 1.0f,
      0.8f,  0.8f, 0.0f, 1.0f,
      -0.8f, -0.8f, 0.0f, 1.0f,
      0.8f, -0.8f, 0.0f, 1.0f
   };

   GLenum ErrorCheckValue = glGetError();

   glGenVertexArrays(1, &VaoId);
   glBindVertexArray(VaoId);

   glGenBuffers(1, &VboId);
   glBindBuffer(GL_ARRAY_BUFFER, VboId);
   glBufferData(GL_ARRAY_BUFFER, sizeof(Vertices), Vertices, GL_STATIC_DRAW);
   glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
   glEnableVertexAttribArray(0);

   ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(
	  stderr,
	  "ERROR: Could not create a VBO: %s \n",
	  gluErrorString(ErrorCheckValue)
	      );

      exit(-1);
   }
}
void DestroyVBO(void)
{
   GLenum ErrorCheckValue = glGetError();

   glDisableVertexAttribArray(1);
   glDisableVertexAttribArray(0);

   glBindBuffer(GL_ARRAY_BUFFER, 0);
   glDeleteBuffers(1, &VboId);

   glBindVertexArray(0);
   glDeleteVertexArrays(1, &VaoId);

   ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(
	  stderr,
	  "ERROR: Could not destroy the VBO: %s \n",
	  gluErrorString(ErrorCheckValue)
	      );

      exit(-1);
   }
}
void CreateShaders(void)
{
   GLenum ErrorCheckValue = glGetError();
   GLfloat ColorUniform[] = {0.0f, 1.0f, 0.0f, 1.0f};

   VertexShaderId = glCreateShader(GL_VERTEX_SHADER);
   glShaderSource(VertexShaderId, 1, &VertexShader, NULL);
   glCompileShader(VertexShaderId);

   FragmentShaderId = glCreateShader(GL_FRAGMENT_SHADER);
   glShaderSource(FragmentShaderId, 1, &FragmentShader, NULL);
   glCompileShader(FragmentShaderId);

   ProgramId = glCreateProgram();
   glAttachShader(ProgramId, VertexShaderId);
   glAttachShader(ProgramId, FragmentShaderId);
   glLinkProgram(ProgramId);
   glUseProgram(ProgramId);

   ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(
	  stderr,
	  "ERROR: Could not create the shaders: %s \n",
	  gluErrorString(ErrorCheckValue)
	      );

      exit(-1);
   }

   /*    http://www.lighthouse3d.com/tutorials/glsl-tutorial/color-example/ */
   GLint colorLocation = glGetUniformLocation(ProgramId, "color");
   glProgramUniform4fv(ProgramId, colorLocation, 1, ColorUniform);
}
void DestroyShaders(void)
{
   GLenum ErrorCheckValue = glGetError();

   glUseProgram(0);

   glDetachShader(ProgramId, VertexShaderId);
   glDetachShader(ProgramId, FragmentShaderId);

   glDeleteShader(FragmentShaderId);
   glDeleteShader(VertexShaderId);

   glDeleteProgram(ProgramId);

   ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(
	  stderr,
	  "ERROR: Could not destroy the shaders: %s \n",
	  gluErrorString(ErrorCheckValue)
	      );

      exit(-1);
   }
}
