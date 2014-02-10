/*
 * Copyright 2013 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_SERVER_H
#define LINTED_SERVER_H

typedef int linted_server_t;

int linted_server(linted_server_t servers[2]);
int linted_server_close(linted_server_t local);

typedef int linted_server_conn_t;

linted_server_conn_t linted_server_connect(int local);
int linted_server_conn_close(linted_server_conn_t local);

#endif                          /* LINTED_SERVER_H */
