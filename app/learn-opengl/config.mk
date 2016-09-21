# simplified init version
VERSION = 0.0.1

# paths
PREFIX  =
MANDIR  = ${PREFIX}/usr/share/man
ETCDIR  = ${PREFIX}/etc
SBINDIR = ${PREFIX}/sbin

# cflags from $(pkg-config --cflags freeglut glew)
# libs from $(pkg-config --libs freeglut glew)
CC = gcc
LD = $(CC)
CPPFLAGS =
CFLAGS   = -Wextra -Wall -Wno-unused-result -Os -I/usr/include/GL -I/usr/include/libdrm
LDFLAGS  = -lglut -lGLEW -lGLU -lGL -lm


