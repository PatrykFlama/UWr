#pragma once

struct RESOURCE;

RESOURCE* allocate_resource();
void free_resource(RESOURCE* handle);

void use_resource(RESOURCE*);
